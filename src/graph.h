#pragma once
#include "calculator.h"
#include <raylib.h>
#include "imgui.h"
#include "imgui_node_editor.h"
namespace ed = ax::NodeEditor;

enum class PinType {
    Flow,
    Bool,
    Int,
    Float,
    String,
    Object,
    Function,
    Delegate,
};
enum class PinKind {
    Output,
    Input,
};
enum class NodeType {
    Blueprint,
    Simple,
    Tree,
    Comment,
    Houdini,
    Function,
};
struct Pin {
    ed::PinId id;   
    struct Node* node;
    std::string name;
    PinType type;
    PinKind kind;

    Pin(int id, const char* name, PinType type) : id(id), node(nullptr), name(name), type(type), kind(PinKind::Input) {
    }

    float GetWidth() const;
    
};
struct Node {
    Node(int id, const char* name, ImColor color = ImColor(255,255,255)) : id(id), name(name), color(color), type(NodeType::Blueprint), size(0, 0), tree(nullptr), err_data{} { }
    ed::NodeId id;
    std::string name;
    std::vector<Pin> inputs;
    std::vector<Pin> outputs;
    ImColor color;
    NodeType type;
    ImVec2 size;
    struct ExpressionTree* tree;
    ErrorData err_data;
    std::string state;
    std::string saved_state;
};
struct Link {
    ed::LinkId id;

    ed::PinId start_pin_id;
    ed::PinId end_pin_id;

    ImColor Color;

    Link(ed::LinkId id, ed::PinId start_pin_id, ed::PinId end_pin_id) : id(id), start_pin_id(start_pin_id), end_pin_id(end_pin_id), Color(255, 255, 255) { }
};


struct NodeEditor {
    NodeEditor();
    ~NodeEditor();
    void Draw(Vector2 win_size);

    Node* SpawnFunctionNode();
    void BuildNode(Node* node);
    void BuildNodes();

    uint32_t GetNextId();
    bool IsConnected(ed::PinId pin_id) const;

    Pin* GetPin(ed::PinId pin_id);

    ed::EditorContext* ctx = nullptr;


    std::vector<Node> nodes;
    std::vector<Link> links;
    uint32_t id_counter;
};


