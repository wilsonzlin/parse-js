import Dagre from "@dagrejs/dagre";
import { Editor } from "@monaco-editor/react";
import {
  Valid,
  Validator,
  VArray,
  VBoolean,
  VFiniteNumber,
  VInteger,
  VMap,
  VMember,
  VOptional,
  VString,
  VStringEnum,
  VStruct,
  VTagged,
  VUnion,
} from "@wzlin/valid";
import UnreachableError from "@xtjs/lib/UnreachableError";
import {
  Edge,
  Handle,
  Node,
  Panel,
  Position,
  ReactFlow,
  ReactFlowProvider,
  useEdgesState,
  useNodesInitialized,
  useNodesState,
  useReactFlow,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import initWasm, { build_js, set_panic_hook } from "optimize-js-debugger";
import { Fragment, useEffect, useMemo, useState } from "react";
import "./App.css";

enum BinOp {
  Add = "Add",
  Div = "Div", // Divide.
  Exp = "Exp", // Exponentiate.
  Geq = "Geq", // Greater than or equals to.
  GetProp = "GetProp",
  Gt = "Gt", // Greater than.
  Leq = "Leq", // Less than or equals to.
  LooseEq = "LooseEq",
  Lt = "Lt", // Less than.
  Mod = "Mod", // Modulo.
  Mul = "Mul", // Multiply.
  NotLooseEq = "NotLooseEq",
  NotStrictEq = "NotStrictEq",
  StrictEq = "StrictEq",
  Sub = "Sub", // Subtract.
}

enum UnOp {
  Neg = "Neg",
  Not = "Not",
  Plus = "Plus",
  Typeof = "Typeof",
  Void = "Void",
}

class VBigInt extends Validator<bigint> {
  constructor() {
    super(0n);
  }
}

const vConst = new VUnion(
  new VMember(["Null", "Undefined"] as const),
  new VStruct({
    BigInt: new VOptional(new VBigInt()),
    Bool: new VOptional(new VBoolean()),
    Num: new VOptional(new VFiniteNumber()),
    Str: new VOptional(new VString()),
  }),
);

type Const = Valid<typeof vConst>;

const vArg = new VStruct({
  Builtin: new VOptional(new VString()),
  Const: new VOptional(vConst),
  Var: new VOptional(new VInteger()),
});

type Arg = Valid<typeof vArg>;

const vCallArg = new VStruct({
  Arg: new VOptional(vArg),
  Spread: new VOptional(vArg),
});

const vInst = new VTagged("$type", {
  Bin: new VStruct({
    tgt: new VInteger(),
    left: vArg,
    op: new VStringEnum(BinOp),
    right: vArg,
  }),
  Un: new VStruct({
    tgt: new VInteger(),
    arg: vArg,
    op: new VStringEnum(UnOp),
  }),
  VarAssign: new VStruct({
    tgt: new VInteger(),
    value: vArg,
  }),
  PropAssign: new VStruct({
    obj: vArg,
    prop: vArg,
    value: vArg,
  }),
  Goto: new VStruct({
    label: new VInteger(),
  }),
  CondGoto: new VStruct({
    cond: vArg,
    label: new VInteger(),
  }),
  NotCondGoto: new VStruct({
    cond: vArg,
    label: new VInteger(),
  }),
  Call: new VStruct({
    tgt: new VOptional(new VInteger()),
    func: vArg,
    this: vArg,
    args: new VArray(vCallArg),
  }),
  ForeignLoad: new VStruct({
    from: new VInteger(),
    to: new VInteger(),
  }),
  ForeignStore: new VStruct({
    from: vArg,
    to: new VInteger(),
  }),
  UnknownLoad: new VStruct({
    from: new VString(),
    to: new VInteger(),
  }),
  UnknownStore: new VStruct({
    from: vArg,
    to: new VString(),
  }),
  Phi: new VStruct({
    tgt: new VInteger(),
    from_blocks: new VMap(new VInteger(), vArg),
  }),
  Label: new VStruct({
    label: new VInteger(),
  }),
});

type Inst = Valid<typeof vInst>;

const vDebugStep = new VStruct({
  name: new VString(),
  bblockOrder: new VArray(new VInteger()),
  bblocks: new VMap(new VInteger(), new VArray(vInst)),
  cfgChildren: new VMap(new VInteger(), new VArray(new VInteger())),
});

type DebugStep = Valid<typeof vDebugStep>;

const vDebug = new VStruct({
  steps: new VArray(vDebugStep),
});

type Debug = Valid<typeof vDebug>;

type BBlockNode = Node<
  {
    label: number;
    insts: Array<Valid<typeof vInst>>;
  },
  "bblock"
>;

const ConstElement = ({ value }: { value: Const }) => {
  if (value === "Null") {
    return <span className="null">null</span>;
  }
  if (value === "Undefined") {
    return <span className="undefined">undefined</span>;
  }
  if (value.BigInt !== undefined) {
    return <span className="bigint">{value.BigInt.toString()}</span>;
  }
  if (value.Bool !== undefined) {
    return <span className="bool">{value.Bool.toString()}</span>;
  }
  if (value.Num !== undefined) {
    return <span className="num">{value.Num}</span>;
  }
  if (value.Str !== undefined) {
    return <span className="str">{value.Str}</span>;
  }
  throw new UnreachableError();
};

const VarElement = ({ id }: { id: number }) => (
  <span className="var">%{id}</span>
);

const ArgElement = ({ arg }: { arg: Arg }) => {
  if (arg.Builtin) {
    return <span className="builtin">{arg.Builtin}</span>;
  }
  if (arg.Const) {
    return <ConstElement value={arg.Const} />;
  }
  if (arg.Var != undefined) {
    return <VarElement id={arg.Var} />;
  }
  throw new UnreachableError();
};

const InstElement = ({ inst }: { inst: Inst }) => {
  switch (inst.$type) {
    case "Bin":
      return (
        <>
          <div>
            <VarElement id={inst.tgt} />
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.left} />
            <span>{inst.op}</span>
            <ArgElement arg={inst.right} />
          </div>
        </>
      );
    case "Call":
      return (
        <>
          <div>
            {inst.tgt == undefined ? <span /> : <VarElement id={inst.tgt} />}
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.func} />
            <span>(</span>
            {inst.args.map((arg, i) => (
              <Fragment key={i}>
                <span>{i === 0 ? "" : ", "}</span>
                {arg.Spread && <span>&hellip;</span>}
                {arg.Arg && <ArgElement arg={arg.Arg} />}
              </Fragment>
            ))}
            <span>)</span>
          </div>
        </>
      );
    case "CondGoto":
      return (
        <>
          <div>
            <span>goto</span>
          </div>
          <div>
            <span className="label">:{inst.label}</span>
            <span> if </span>
            <ArgElement arg={inst.cond} />
          </div>
        </>
      );
    case "ForeignLoad":
      return (
        <>
          <div>
            <VarElement id={inst.to} />
            <span> =</span>
          </div>
          <div>
            <span className="foreign">foreign {inst.from}</span>
          </div>
        </>
      );
    case "ForeignStore":
      return (
        <>
          <div>
            <span className="foreign">foreign {inst.to}</span>
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.from} />
          </div>
        </>
      );
    case "Goto":
      return (
        <>
          <div>
            <span>goto</span>
          </div>
          <div>
            <span className="label">:{inst.label}</span>
          </div>
        </>
      );
    case "Label":
      throw new UnreachableError();
    case "NotCondGoto":
      return (
        <>
          <div>
            <span>goto</span>
          </div>
          <div>
            <span className="label">:{inst.label}</span>
            <span> if not </span>
            <ArgElement arg={inst.cond} />
          </div>
        </>
      );
    case "Phi":
      return (
        <>
          <div>
            <VarElement id={inst.tgt} />
            <span> =</span>
          </div>
          <div>
            <span>ϕ(</span>
            {[...inst.from_blocks].map(([label, arg], i) => (
              <Fragment key={i}>
                <span>{i === 0 ? "" : ", "}</span>
                <span className="label">:{label}</span>
                <ArgElement arg={arg} />
              </Fragment>
            ))}
            <span>)</span>
          </div>
        </>
      );
    case "PropAssign":
      return (
        <>
          <div>
            <ArgElement arg={inst.obj} />
            <span>[</span>
            <ArgElement arg={inst.prop} />
            <span>]</span>
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.value} />
          </div>
        </>
      );
    case "Un":
      return (
        <>
          <div>
            <VarElement id={inst.tgt} />
            <span> =</span>
          </div>
          <div>
            <span>{inst.op}</span>
            <ArgElement arg={inst.arg} />
          </div>
        </>
      );
    case "UnknownLoad":
      return (
        <>
          <div>
            <VarElement id={inst.to} />
            <span> =</span>
          </div>
          <div>
            <span className="unknown">unknown {inst.from}</span>
          </div>
        </>
      );
    case "UnknownStore":
      return (
        <>
          <div>
            <span className="unknown">unknown {inst.to}</span>
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.from} />
          </div>
        </>
      );
    case "VarAssign":
      return (
        <>
          <div>
            <VarElement id={inst.tgt} />
            <span> =</span>
          </div>
          <div>
            <ArgElement arg={inst.value} />
          </div>
        </>
      );
  }
};

const BBlockElement = ({
  data: { label, insts },
}: {
  data: BBlockNode["data"];
}) => {
  return (
    <>
      <Handle type="target" position={Position.Top} />
      <div className="bblock">
        <h1>:{label}</h1>
        <ol className="insts">
          {insts.map((s, i) => (
            <li key={i} className="inst">
              <InstElement inst={s} />
            </li>
          ))}
        </ol>
      </div>
      <Handle type="source" position={Position.Bottom} />
    </>
  );
};

const getLayoutedElements = (
  nodes: Array<BBlockNode>,
  edges: Array<Edge>,
  options: { direction: string },
) => {
  const g = new Dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: options.direction });

  for (const edge of edges) {
    g.setEdge(edge.source, edge.target);
  }
  for (const node of nodes) {
    g.setNode(node.id, {
      ...node,
      width: node.measured?.width ?? 0,
      height: node.measured?.height ?? 0,
    });
  }

  Dagre.layout(g);

  return {
    nodes: nodes.map((node) => {
      const position = g.node(node.id);
      // We are shifting the Dagre node position (anchor=center center) to the top left
      // so it matches the React Flow node anchor point (top left).
      const x = position.x - (node.measured?.width ?? 0) / 2;
      const y = position.y - (node.measured?.height ?? 0) / 2;
      return { ...node, position: { x, y } };
    }),
    edges,
  };
};

const nodeTypes = {
  bblock: BBlockElement,
};

const Graph = ({
  stepNames,
  step,
}: {
  stepNames: Array<string>;
  step: DebugStep;
}) => {
  const initNodes = useMemo(
    () =>
      step.bblockOrder.map<BBlockNode>((label) => ({
        id: `${label}`,
        type: "bblock",
        data: {
          label,
          insts: step.bblocks.get(label)!,
        },
        position: { x: 0, y: 0 },
      })),
    [step],
  );
  const initEdges = useMemo(
    () =>
      [...step.cfgChildren].flatMap(([src, dests]) =>
        dests.map<Edge>((dest) => ({
          id: `${src}-${dest}`,
          source: `${src}`,
          target: `${dest}`,
          animated: true,
        })),
      ),
    [step],
  );

  const { fitView } = useReactFlow();
  const [nodes, setNodes, onNodesChange] = useNodesState(initNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState(initEdges);
  // https://github.com/xyflow/xyflow/issues/533#issuecomment-1601814350
  const nodesSized = useNodesInitialized();
  const [layoutCalculated, setLayoutCalculated] = useState(false);
  // Force update nodes and edges when source code or step changes.
  // (Otherwise, new nodes and edges are left unused.)
  useEffect(() => {
    setNodes(initNodes);
    setEdges(initEdges);
    setLayoutCalculated(false);
  }, [step]);

  useEffect(() => {
    if (!nodesSized || layoutCalculated) {
      return;
    }
    const layouted = getLayoutedElements(nodes, edges, { direction: "TB" });
    setNodes(layouted.nodes);
    setEdges(layouted.edges);
    setLayoutCalculated(true);
  }, [
    // WARNING: This must *NOT* run when `nodes` or `edges` change, as they will have a size of 0 but nodesSized will be true.
    // This is correct anyway: we only run after sizing, not before (when inputs change) or after (when layout is calculated).
    nodesSized,
  ]);

  useEffect(() => {
    if (nodesSized && layoutCalculated) {
      fitView();
    }
  }, [layoutCalculated]);

  return (
    <ReactFlow
      edges={edges}
      fitView
      nodes={nodes}
      nodesDraggable={false}
      nodeTypes={nodeTypes}
      onEdgesChange={onEdgesChange}
      onNodesChange={onNodesChange}
    >
      <Panel position="top-left">
        <ul className="step-names">
          {stepNames.map((name, i) => (
            <li key={i} className={name == step.name ? "current" : ""}>
              {name}
            </li>
          ))}
        </ul>
      </Panel>
    </ReactFlow>
  );
};

export const App = ({}: {}) => {
  const [data, setData] = useState<Debug>();
  const [stepIdx, setStepIdx] = useState(0);
  const stepNames = useMemo(() => data?.steps.map((s) => s.name) ?? [], [data]);

  useEffect(() => {
    (async () => {
      await initWasm({
        module_or_path: "/optimize_js_debugger_bg.wasm",
      });
      set_panic_hook();
    })();
  }, []);

  useEffect(() => {
    const listener = (e: KeyboardEvent) => {
      if (e.key === "ArrowLeft" || e.key === "ArrowUp") {
        setStepIdx((idx) => Math.max(0, idx - 1));
      } else if (e.key === "ArrowRight" || e.key === "ArrowDown") {
        setStepIdx((idx) => Math.min((data?.steps.length ?? 1) - 1, idx + 1));
      }
    };
    window.addEventListener("keydown", listener);
    return () => window.removeEventListener("keydown", listener);
  }, [data]);

  return (
    <div className="App">
      <main>
        <div className="canvas">
          {data && (
            <ReactFlowProvider>
              <Graph step={data.steps[stepIdx]} stepNames={stepNames} />
            </ReactFlowProvider>
          )}
        </div>
        <div className="pane">
          <div className="editor">
            <Editor
              height="95vh"
              width="40vw"
              defaultLanguage="javascript"
              defaultValue="let x = 1; if (x) { g(); } f(x);"
              onChange={(e) => {
                const source = e?.trim() ?? "";
                // TODO Global mode.
                const res = build_js(source, false);
                console.log("Built JS:", res);
                // TODO AST
                setData(vDebug.parseRoot(res.debug));
              }}
            />
          </div>
        </div>
      </main>
    </div>
  );
};
