import { Valid, Validator, VArray, VBoolean, VFiniteNumber, VInteger, VMap, VMember, VObjectMap, VOptional, VString, VStringEnum, VStruct, VTagged, VUnion } from "@wzlin/valid";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Edge, Handle, Node, Panel, Position, ReactFlow, ReactFlowProvider, useEdgesState, useNodesState, useReactFlow } from "@xyflow/react";
import Dagre from "@dagrejs/dagre";
import initWasm, { build_js, set_panic_hook } from "optimize-js-debugger";
import { Editor } from "@monaco-editor/react";
import "./App.css";
import "@xyflow/react/dist/style.css";

enum BinOp {
  Add = "Add",
  Div = "Div", // Divide.
  Exp = "Exp", // Exponentiate.
  Geq = "Geq", // Greater than or equals to.
  GetProp = "GetProp",
  Gt = "Gt",  // Greater than.
  Leq = "Leq", // Less than or equals to.
  LooseEq = "LooseEq",
  Lt = "Lt",  // Less than.
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

const vArg = new VStruct({
  Builtin: new VOptional(new VString()),
  Const: new VOptional(vConst),
  Var: new VOptional(new VInteger()),
});

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

type BBlockNode = Node<{
  label: number,
  insts: Array<Valid<typeof vInst>>,
}, "bblock">;

const BBlockElement = ({ data: { label, insts } }: {data: BBlockNode["data"]}) => {
  return (
    <>
      <Handle type="target" position={Position.Top} />
      <div className="bblock">
        <h1>:{label}</h1>
        <ol>
          {insts.map((s, i) => (
            <li key={i}>{JSON.stringify(s)}</li>
          ))}
        </ol>
      </div>
      <Handle type="source" position={Position.Bottom} />
    </>
  )
};

const getLayoutedElements = (
  nodes: Array<BBlockNode>,
  edges: Array<Edge>,
  options: { direction: string },
) => {
  const g = new Dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: options.direction });

  for (const edge of edges) {
    g.setEdge(edge.source, edge.target)
  }
  for (const node of nodes) {
    g.setNode(node.id, {
      ...node,
      width: node.measured?.width ?? 0,
      height: node.measured?.height ?? 0,
    })
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
  step,
}: {
  step: DebugStep
}) => {
  const initNodes = useMemo(() => step.bblockOrder.map<BBlockNode>(label => ({
    id: `${label}`,
    type: "bblock",
    data: {
      label,
      insts: step.bblocks.get(label)!,
    },
    position: { x: 0, y: 0 },
  })), [step]);
  const initEdges = useMemo(() => [...step.cfgChildren].flatMap(([src, dests]) => dests.map<Edge>(dest => ({
    id: `${src}-${dest}`,
    source: `${src}`,
    target: `${dest}`,
    animated: true,
  }))), [step]);

  const [tick, setTick] = useState(0);

  const {fitView} = useReactFlow();
  const [nodes, setNodes, onNodesChange] = useNodesState(initNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState(initEdges);
  // Force update nodes and edges when source code or step changes.
  // (Otherwise, new nodes and edges are left unused.)
  useEffect(() => {
    setNodes(initNodes);
    setEdges(initEdges);
    // We can't call onLayout now as the nodes don't have correct DOM sizes yet for it to calculate the layout.
    const timeout = setTimeout(() => {
      setTick(tick + 1);
    }, 100);
    return () => clearTimeout(timeout);
  }, [step]);

  const layoutRaf = useRef<number | null>(null);
  const onLayout = useCallback((direction: "TB" | "LR") => {
    const layouted = getLayoutedElements(nodes, edges, { direction });

    setNodes([...layouted.nodes]);
    setEdges([...layouted.edges]);

    cancelAnimationFrame(layoutRaf.current!);
    layoutRaf.current = requestAnimationFrame(() => {
      fitView();
    });
  }, [nodes, edges]);
  useEffect(() => onLayout("TB"), [tick]);

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
          <h1>{step.name}</h1>
        </Panel>
      </ReactFlow>
  )
};

export const App = ({}: {}) => {
  const [data, setData] = useState<Debug>();
  const [stepIdx, setStepIdx] = useState(0);

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
      if (e.key === "ArrowLeft") {
        setStepIdx((idx) => Math.max(0, idx - 1));
      } else if (e.key === "ArrowRight") {
        setStepIdx((idx) => Math.min(data?.steps.length ?? 0, idx + 1));
      }
    };
    window.addEventListener("keydown", listener);
    return () => window.removeEventListener("keydown", listener);
  }, [data]);

  return (
    <div className="App">
      <main>
        <div className="canvas">
          {data &&
          <ReactFlowProvider>
          <Graph step={data.steps[stepIdx]} />
          </ReactFlowProvider>
}
        </div>
        <div className="pane">
          <div className="editor">
            <Editor
              height="95vh"
              width="40vw"
              defaultLanguage="javascript"
              defaultValue="let x = 1; if (x) { g(); } f(x);"
              onChange={e => {
                const source = e?.trim() ?? "";
                // TODO Global mode.
                const res = build_js(source, false);
                console.log("Built JS:", res)
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
