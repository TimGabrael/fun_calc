#pragma once
#include "calculator.h"
#include <raylib.h>
#include "imgui.h"
#include "imgui_node_editor.h"
namespace ed = ax::NodeEditor;

struct FunctionGraph {
    FunctionGraph(Vector2 win_size);
    ~FunctionGraph();

    struct ExpressionTree* tree;
    VariableData variables;
    ErrorData err;
    std::string content;
    std::string id;

    Vector2 position;
    Vector2 size;

    
    void Draw();
};

enum class PinType {
    Float,
    String,
};
enum class PinKind {
    Output,
    Input,
};
enum class NodeType {
    Simple,
};
struct Pin {
    ed::PinId id;   
    struct Node* node;
    std::string name;
    PinType type;
    PinKind kind;
    
};
struct Node {
    Node(int id, const char* name, ImColor color = ImColor(255,255,255)) : id(id), name(name), color(color), type(NodeType::Simple), size(0, 0) { }
    ed::NodeId id;
    std::string name;
    std::vector<Pin> inputs;
    std::vector<Pin> outputs;
    ImColor color;
    NodeType type;
    ImVec2 size;
    std::string state;
    std::string saved_state;
};
struct Link
{
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

    Node* SpawnTestNode();
    void BuildNode(Node* node);

    uint32_t GetNodeId();
    uint32_t GetPinId();
    uint32_t GetLinkId();

    ed::EditorContext* ctx = nullptr;


    std::vector<Node> nodes;
    uint32_t node_id_counter;
    uint32_t pin_id_counter;
    uint32_t link_id_counter;
};


