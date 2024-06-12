#include "graph.h"
#include "calculator.h"
#include "imgui.h"
#include "imgui_node_editor.h"
#include "imgui_stdlib.h"
#include <iostream>

static size_t ID_COUNTER = 0;

FunctionGraph::FunctionGraph(Vector2 position) {
    this->id = std::to_string(ID_COUNTER);
    this->tree = nullptr;
    ID_COUNTER += 1;
    this->err.info = "";
    this->err.failed = false;
    this->position = position;
    this->size = {400.0f, 400.0f};
}
FunctionGraph::~FunctionGraph() {
}
void FunctionGraph::Draw() {
    ImGui::SetNextWindowPos({position.x, position.y}, ImGuiCond_::ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSize({size.x, size.y}, ImGuiCond_::ImGuiCond_FirstUseEver);

    ImGui::Begin(this->id.c_str());
    ImGui::PushItemWidth(-1.0f);
    if(ImGui::InputText("##input", &this->content, ImGuiInputTextFlags_::ImGuiInputTextFlags_EnterReturnsTrue)) {
        if(this->tree) {
            DeleteExpressionTree(this->tree);
            this->tree = nullptr;
        }
        bool invalid = false;
        for(char c : this->content) {
            if(c < 0) {
                invalid = true;
                break;
            }
        }
        if(invalid) {
            this->err.info = "contains invalid character";
            this->err.failed = true;
        }
        else {
            this->err = ParseFunction(this->content, &this->tree);
        }
    }
    ImGui::PopItemWidth();

    if(this->err.failed) {
        ImGui::PushStyleColor(ImGuiCol_::ImGuiCol_Text, 0xFF0000FF);
        ImGui::PushTextWrapPos();
        ImGui::Text("%s", this->err.info.c_str());
        ImGui::PopStyleColor();
        ImGui::PopTextWrapPos();
    }

    ImGui::End();

}


static void ImGuiEx_BeginColumn() {
    ImGui::BeginGroup();
}
static void ImGuiEx_NextColumn() {
    ImGui::EndGroup();
    ImGui::SameLine();
    ImGui::BeginGroup();
}
static void ImGuiEx_EndColumn() {
    ImGui::EndGroup();
}


NodeEditor::NodeEditor() {
    ed::Config config;
    config.SettingsFile = "TestEditor.json";
    this->ctx = ed::CreateEditor(&config);
    this->node_id_counter = 0;
    this->pin_id_counter = 0;
    this->link_id_counter = 0;
}
NodeEditor::~NodeEditor() {
    ed::DestroyEditor(this->ctx);
    ImGui::BeginGroup();
}
void NodeEditor::Draw(Vector2 win_size) {
    ed::SetCurrentEditor(this->ctx);
    ed::Begin("node_editor");
    ImGui::SetWindowSize({win_size.x, win_size.y});
    ImGui::SetWindowPos({0.0f, 0.0f});

    int uniqueId = 1;

    ed::NodeId nodeA_Id = uniqueId++;
    ed::PinId  nodeA_InputPinId = uniqueId++;
    ed::PinId  nodeA_OutputPinId = uniqueId++;

    ed::BeginNode(nodeA_Id);
    ImGui::Text("Node A");
    ed::BeginPin(nodeA_InputPinId, ed::PinKind::Input);
    ImGui::Bullet();
    ed::EndPin();
    ImGui::SameLine();
    ed::BeginPin(nodeA_OutputPinId, ed::PinKind::Output);
    ImGui::Bullet();
    
    ed::EndPin();
    ed::EndNode();




    ed::End();
}

Node* NodeEditor::SpawnTestNode() {
    this->nodes.emplace_back(GetNodeId(), "whatever", ImColor(255, 128, 128));
    BuildNode(&this->nodes.back());
    return &this->nodes.back();
}
void NodeEditor::BuildNode(Node* node) {
    for(Pin& in : node->inputs) {
        in.node = node;
        in.kind = PinKind::Input;
    }
    for(Pin& out : node->outputs) {
        out.node = node;
        out.kind = PinKind::Output;
    }
}
uint32_t NodeEditor::GetNodeId() {
    return this->node_id_counter++;
}
uint32_t NodeEditor::GetPinId() {
    return this->pin_id_counter++;
}
uint32_t NodeEditor::GetLinkId() {
    return this->link_id_counter++;
}

