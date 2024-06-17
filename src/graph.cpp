#include "graph.h"
#include "calculator.h"
#include "imgui.h"
#include "imgui_node_editor.h"
#include "imgui_stdlib.h"
#include "imgui_internal.h"
#include "util.h"
#include <iostream>

static size_t ID_COUNTER = 0;
static float PIN_ICON_SIZE = 24.0f;

float Pin::GetWidth() const {
    ImVec2 text_size = ImGui::CalcTextSize(name.c_str());
    return text_size.x + PIN_ICON_SIZE;
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
enum class IconType : uint32_t { 
    Flow, 
    Circle,
    Square,
    Grid,
    RoundSquare,
    Diamond
};
static bool Splitter(bool split_vertically, float thickness, float* size1, float* size2, float min_size1, float min_size2, float splitter_long_axis_size = -1.0f)
{
    using namespace ImGui;
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;
    ImGuiID id = window->GetID("##Splitter");
    ImRect bb;
    bb.Min = split_vertically ? ImVec2(*size1 + window->DC.CursorPos.x, 0.0f + window->DC.CursorPos.y) : ImVec2(0.0f + window->DC.CursorPos.x, *size1 + window->DC.CursorPos.y);
    ImVec2 calc_item_size = CalcItemSize(split_vertically ? ImVec2(thickness, splitter_long_axis_size) : ImVec2(splitter_long_axis_size, thickness), 0.0f, 0.0f);
    bb.Max = ImVec2(bb.Min.x + calc_item_size.x, bb.Min.y + calc_item_size.y);
    return SplitterBehavior(bb, id, split_vertically ? ImGuiAxis_X : ImGuiAxis_Y, size1, size2, min_size1, min_size2, 0.0f);
}
static void DrawIcon(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, IconType type, bool filled, ImU32 color, ImU32 innerColor) {
    auto rect           = ImRect(a, b);
    auto rect_x         = rect.Min.x;
    auto rect_y         = rect.Min.y;
    auto rect_w         = rect.Max.x - rect.Min.x;
    auto rect_h         = rect.Max.y - rect.Min.y;
    auto rect_center_x  = (rect.Min.x + rect.Max.x) * 0.5f;
    auto rect_center_y  = (rect.Min.y + rect.Max.y) * 0.5f;
    auto rect_center    = ImVec2(rect_center_x, rect_center_y);
    const auto outline_scale  = rect_w / 24.0f;
    const auto extra_segments = static_cast<int>(2 * outline_scale); // for full circle

    if (type == IconType::Flow)
    {
        const auto origin_scale = rect_w / 24.0f;

        const auto offset_x  = 1.0f * origin_scale;
        const auto offset_y  = 0.0f * origin_scale;
        const auto margin     = (filled ? 2.0f : 2.0f) * origin_scale;
        const auto rounding   = 0.1f * origin_scale;
        const auto tip_round  = 0.7f; // percentage of triangle edge (for tip)
                                      //const auto edge_round = 0.7f; // percentage of triangle edge (for corner)
        const auto canvas = ImRect(
                rect.Min.x + margin + offset_x,
                rect.Min.y + margin + offset_y,
                rect.Max.x - margin + offset_x,
                rect.Max.y - margin + offset_y);
        const auto canvas_x = canvas.Min.x;
        const auto canvas_y = canvas.Min.y;
        const auto canvas_w = canvas.Max.x - canvas.Min.x;
        const auto canvas_h = canvas.Max.y - canvas.Min.y;

        const auto left   = canvas_x + canvas_w            * 0.5f * 0.3f;
        const auto right  = canvas_x + canvas_w - canvas_w * 0.5f * 0.3f;
        const auto top    = canvas_y + canvas_h            * 0.5f * 0.2f;
        const auto bottom = canvas_y + canvas_h - canvas_h * 0.5f * 0.2f;
        const auto center_y = (top + bottom) * 0.5f;
        //const auto angle = AX_PI * 0.5f * 0.5f * 0.5f;

        const auto tip_top    = ImVec2(canvas_x + canvas_w * 0.5f, top);
        const auto tip_right  = ImVec2(right, center_y);
        const auto tip_bottom = ImVec2(canvas_x + canvas_w * 0.5f, bottom);

        drawList->PathLineTo(ImVec2(left, top + rounding));
        drawList->PathBezierCubicCurveTo(
                ImVec2(left, top),
                ImVec2(left, top),
                ImVec2(left + rounding, top));
        drawList->PathLineTo(tip_top);
        drawList->PathLineTo(ImVec2(tip_top.x + (tip_right.x-tip_top.x) * tip_round, tip_top.y + (tip_right.y - tip_top.y) * tip_round));
        drawList->PathBezierCubicCurveTo(
                tip_right,
                tip_right,
                ImVec2(tip_bottom.x + (tip_right.x - tip_bottom.x) * tip_round, tip_bottom.y + (tip_right.y - tip_bottom.y) * tip_round));
        drawList->PathLineTo(tip_bottom);
        drawList->PathLineTo(ImVec2(left + rounding, bottom));
        drawList->PathBezierCubicCurveTo(
                ImVec2(left, bottom),
                ImVec2(left, bottom),
                ImVec2(left, bottom - rounding));

        if (!filled)
        {
            if (innerColor & 0xFF000000)
                drawList->AddConvexPolyFilled(drawList->_Path.Data, drawList->_Path.Size, innerColor);

            drawList->PathStroke(color, true, 2.0f * outline_scale);
        }
        else
            drawList->PathFillConvex(color);
    }
    else
    {
        auto triangleStart = rect_center_x + 0.32f * rect_w;

        auto rect_offset = -static_cast<int>(rect_w * 0.25f * 0.25f);

        rect.Min.x    += rect_offset;
        rect.Max.x    += rect_offset;
        rect_x        += rect_offset;
        rect_center_x += rect_offset * 0.5f;
        rect_center.x += rect_offset * 0.5f;

        if (type == IconType::Circle)
        {
            const auto c = rect_center;

            if (!filled)
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;

                if (innerColor & 0xFF000000)
                    drawList->AddCircleFilled(c, r, innerColor, 12 + extra_segments);
                drawList->AddCircle(c, r, color, 12 + extra_segments, 2.0f * outline_scale);
            }
            else
            {
                drawList->AddCircleFilled(c, 0.5f * rect_w / 2.0f, color, 12 + extra_segments);
            }
        }

        if (type == IconType::Square)
        {
            if (filled)
            {
                const auto r  = 0.5f * rect_w / 2.0f;
                const auto p0 = ImVec2(rect_center.x - r, rect_center.y - r);
                const auto p1 = ImVec2(rect_center.x + r, rect_center.y + r);

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRectFilled(p0, p1, color, 0, ImDrawFlags_RoundCornersAll);
#else
                drawList->AddRectFilled(p0, p1, color, 0, 15);
#endif
            }
            else
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;
                const auto p0 = ImVec2(rect_center.x - r, rect_center.y - r);
                const auto p1 = ImVec2(rect_center.x + r, rect_center.y + r);

                if (innerColor & 0xFF000000)
                {
#if IMGUI_VERSION_NUM > 18101
                    drawList->AddRectFilled(p0, p1, innerColor, 0, ImDrawFlags_RoundCornersAll);
#else
                    drawList->AddRectFilled(p0, p1, innerColor, 0, 15);
#endif
                }

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRect(p0, p1, color, 0, ImDrawFlags_RoundCornersAll, 2.0f * outline_scale);
#else
                drawList->AddRect(p0, p1, color, 0, 15, 2.0f * outline_scale);
#endif
            }
        }

        if (type == IconType::Grid)
        {
            const auto r = 0.5f * rect_w / 2.0f;
            const auto w = ceilf(r / 3.0f);

            const auto baseTl = ImVec2(floorf(rect_center_x - w * 2.5f), floorf(rect_center_y - w * 2.5f));
            const auto baseBr = ImVec2(floorf(baseTl.x + w), floorf(baseTl.y + w));

            auto tl = baseTl;
            auto br = baseBr;
            for (int i = 0; i < 3; ++i)
            {
                tl.x = baseTl.x;
                br.x = baseBr.x;
                drawList->AddRectFilled(tl, br, color);
                tl.x += w * 2;
                br.x += w * 2;
                if (i != 1 || filled)
                    drawList->AddRectFilled(tl, br, color);
                tl.x += w * 2;
                br.x += w * 2;
                drawList->AddRectFilled(tl, br, color);

                tl.y += w * 2;
                br.y += w * 2;
            }

            triangleStart = br.x + w + 1.0f / 24.0f * rect_w;
        }

        if (type == IconType::RoundSquare)
        {
            if (filled)
            {
                const auto r  = 0.5f * rect_w / 2.0f;
                const auto cr = r * 0.5f;
                const auto p0 = ImVec2(rect_center.x - r, rect_center.y - r);
                const auto p1 = ImVec2(rect_center.x + r, rect_center.y + r);

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRectFilled(p0, p1, color, cr, ImDrawFlags_RoundCornersAll);
#else
                drawList->AddRectFilled(p0, p1, color, cr, 15);
#endif
            }
            else
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;
                const auto cr = r * 0.5f;
                const auto p0 = ImVec2(rect_center.x - r, rect_center.y - r);
                const auto p1 = ImVec2(rect_center.x + r, rect_center.y + r);

                if (innerColor & 0xFF000000)
                {
#if IMGUI_VERSION_NUM > 18101
                    drawList->AddRectFilled(p0, p1, innerColor, cr, ImDrawFlags_RoundCornersAll);
#else
                    drawList->AddRectFilled(p0, p1, innerColor, cr, 15);
#endif
                }

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRect(p0, p1, color, cr, ImDrawFlags_RoundCornersAll, 2.0f * outline_scale);
#else
                drawList->AddRect(p0, p1, color, cr, 15, 2.0f * outline_scale);
#endif
            }
        }
        else if (type == IconType::Diamond)
        {
            if (filled)
            {
                const auto r = 0.607f * rect_w / 2.0f;
                const auto c = rect_center;

                drawList->PathLineTo(ImVec2(c.x +  0, c.y + -r));
                drawList->PathLineTo(ImVec2(c.x +  r, c.y +  0));
                drawList->PathLineTo(ImVec2(c.x +  0, c.y +  r));
                drawList->PathLineTo(ImVec2(c.x + -r, c.y +  0));
                drawList->PathFillConvex(color);
            }
            else
            {
                const auto r = 0.607f * rect_w / 2.0f - 0.5f;
                const auto c = rect_center;

                drawList->PathLineTo(ImVec2(c.x +  0, c.y + -r));
                drawList->PathLineTo(ImVec2(c.x +  r, c.y +  0));
                drawList->PathLineTo(ImVec2(c.x +  0, c.y +  r));
                drawList->PathLineTo(ImVec2(c.x + -r, c.y +  0));

                if (innerColor & 0xFF000000)
                    drawList->AddConvexPolyFilled(drawList->_Path.Data, drawList->_Path.Size, innerColor);

                drawList->PathStroke(color, true, 2.0f * outline_scale);
            }
        }
        else
        {
            const auto triangleTip = triangleStart + rect_w * (0.45f - 0.32f);

            drawList->AddTriangleFilled(
                    ImVec2(ceilf(triangleTip), rect_y + rect_h * 0.5f),
                    ImVec2(triangleStart, rect_center_y + 0.15f * rect_h),
                    ImVec2(triangleStart, rect_center_y - 0.15f * rect_h),
                    color);
        }
    }
}

static void Icon(const ImVec2& size, IconType type, bool filled, const ImVec4& color/* = ImVec4(1, 1, 1, 1)*/, const ImVec4& innerColor/* = ImVec4(0, 0, 0, 0)*/)
{
    if (ImGui::IsRectVisible(size))
    {
        auto cursorPos = ImGui::GetCursorScreenPos();
        auto drawList  = ImGui::GetWindowDrawList();
        DrawIcon(drawList, cursorPos, ImVec2(cursorPos.x + size.x, cursorPos.y + size.y), type, filled, ImColor(color), ImColor(innerColor));
    }
    ImGui::Dummy(size);
}
static ImColor GetIconColor(PinType type)
{
    switch (type)
    {
        default:
        case PinType::Flow:     return ImColor(255, 255, 255);
        case PinType::Bool:     return ImColor(220,  48,  48);
        case PinType::Int:      return ImColor( 68, 201, 156);
        case PinType::Float:    return ImColor(147, 226,  74);
        case PinType::String:   return ImColor(124,  21, 153);
        case PinType::Object:   return ImColor( 51, 150, 215);
        case PinType::Function: return ImColor(218,   0, 183);
        case PinType::Delegate: return ImColor(255,  48,  48);
    }
};
static void DrawPinIcon(const Pin& pin, bool connected, int alpha)
{
    IconType iconType;
    ImColor  color = GetIconColor(pin.type);
    color.Value.w = alpha / 255.0f;
    switch (pin.type)
    {
        case PinType::Flow:     iconType = IconType::Flow;   break;
        case PinType::Bool:     iconType = IconType::Circle; break;
        case PinType::Int:      iconType = IconType::Circle; break;
        case PinType::Float:    iconType = IconType::Circle; break;
        case PinType::String:   iconType = IconType::Circle; break;
        case PinType::Object:   iconType = IconType::Circle; break;
        case PinType::Function: iconType = IconType::Circle; break;
        case PinType::Delegate: iconType = IconType::Square; break;
        default:
            return;
    }
    Icon(ImVec2(PIN_ICON_SIZE, PIN_ICON_SIZE), iconType, connected, color, ImColor(32, 32, 32, alpha));
};



NodeEditor::NodeEditor() {
    ed::Config config;
    config.SettingsFile = "TestEditor.json";
    this->ctx = ed::CreateEditor(&config);
    ed::SetCurrentEditor(this->ctx);
    this->id_counter = 1;

    Node* node = SpawnFunctionNode();
    ed::SetNodePosition(node->id, ImVec2(-250.0f, 220.0f));
    node = SpawnPlotNode();
    ed::SetNodePosition(node->id, ImVec2(-250.0f + 450.0f, 220.0f));
    ed::NavigateToContent();
}
NodeEditor::~NodeEditor() {
    ed::DestroyEditor(this->ctx);
}
void NodeEditor::Draw(Vector2 win_size) {
    ed::SetCurrentEditor(this->ctx);

    ed::Begin("node_editor");
    ImGui::SetWindowSize({win_size.x, win_size.y});
    ImGui::SetWindowPos({0.0f, 0.0f});


    ImVec2 cursor_pos = ImGui::GetCursorScreenPos();

    for(Node& node : this->nodes) {
        ed::BeginNode(node.id);
        ImGui::Text("%s", node.name.c_str());
        ImVec2 node_size = ed::GetNodeSize(node.id);
        ImVec2 node_pos = ed::GetNodePosition(node.id);

        if(node.type == NodeType::Function) {
            static constexpr float text_width = 400.0f;
            ImGui::SetNextItemWidth(text_width);
            if(ImGui::InputText(("##" + std::to_string(node.id.Get())).c_str(), &node.state, ImGuiInputTextFlags_::ImGuiInputTextFlags_EnterReturnsTrue)) {
                if(node.tree) {
                    DeleteExpressionTree(node.tree);
                    node.tree = nullptr;
                }
                bool invalid_characters = false;
                for(char c : node.state) {
                    if(c < 0) {
                        invalid_characters = true;
                    }
                }
                if(!invalid_characters) {
                    node.err_data = ParseFunction(node.state, &node.tree);
                    if(!node.err_data.failed) {
                        node.inputs.clear();
                        VariableData variables = GetVariablesInExpressionTree(node.tree);
                        for(auto& var : variables.variables) {
                            node.inputs.emplace_back(GetNextId(), var.first.c_str(), PinType::Float);
                        }
                        BuildNode(&node);
                    }
                }
                else {
                    node.err_data.failed = true;
                    node.err_data.info = "invalid character in input";
                }
                
            }
            if(node.err_data.failed) {
                ImGui::PushStyleColor(ImGuiCol_::ImGuiCol_Text, 0xFF0000FF);
                ImGui::Text("%s", node.err_data.info.c_str());
                ImGui::PopStyleColor();
            }
        }
        else if(node.type == NodeType::Plot) {
            // TODO: Traverse all connections to the other node and generate the variable data for each instance of the variables
            // then find the (hopefully) only missing input and provide a range for this one 
            // maybe even add a slider to show different points in the graph at different intervals
            static constexpr float plot_size = 400.0f;
            float plot_values[100] = {};
            Node* connection = GetConnectedNode(node.inputs.at(0).id);
            if(connection) {
                if(connection->type == NodeType::Function && connection->tree) {
                    VariableData variables;
                    variables.variables["x"] = 0.0f;
                    for(size_t i = 0; i < ARRSIZE(plot_values); ++i) {
                        variables.variables["x"] = i * 0.1f; // only support x as a little test
                        ErrorData err = EvaluateExpressionTree(connection->state, connection->tree, variables, plot_values[i]);
                        if(err.failed) {
                            connection = nullptr;
                            break;
                        }
                    }
                }
                if(connection) {
                    ImGui::PlotLines(("##" + std::to_string(node.id.Get())).c_str(), plot_values, ARRSIZE(plot_values), 0, NULL, FLT_MAX, FLT_MAX, ImVec2(plot_size, plot_size));
                }
            }

        }
        ImGui::NewLine();
        const float start_cursor_x = ImGui::GetCursorPosX();
        const float padding_x = start_cursor_x - node_pos.x;

        ImGui::BeginGroup();
        for(Pin& in : node.inputs) {
            ed::BeginPin(in.id, ed::PinKind::Input);

            DrawPinIcon(in, this->IsConnected(in.id), 255);
            ImGui::SameLine();
            ImGui::Text("%s", in.name.c_str());

            ed::EndPin();
        }
        ImGui::EndGroup();

        ImGui::SameLine();

        ImGui::BeginGroup();
        for(Pin& out : node.outputs) {
            const float width = out.GetWidth();
            // 25.0f seems to be the padding
            float cx = start_cursor_x + node_size.x - width - padding_x * 2.0f - 10.0f;
            float cy = ImGui::GetCursorPosY();
            ImGui::SetCursorPosX(cx);

            ed::BeginPin(out.id, ed::PinKind::Output);
            ed::PinPivotRect({cx + width - PIN_ICON_SIZE, cy}, {cx + width, cy + PIN_ICON_SIZE});

            ImGui::Text("%s", out.name.c_str());
            ImGui::SameLine();
            DrawPinIcon(out, this->IsConnected(out.id), 255);

            ed::EndPin();
        }
        ImGui::EndGroup();

        ed::EndNode();
    }

    for(Link& link : this->links) {
        ed::Link(link.id, link.start_pin_id, link.end_pin_id);
    }

    ed::BeginCreate();
    ed::PinId start_pin_id, end_pin_id = 0;
    if(ed::QueryNewLink(&start_pin_id, &end_pin_id)) {
        if(ed::AcceptNewItem()) {
            Pin* start_pin = this->GetPin(start_pin_id);
            Pin* end_pin = this->GetPin(end_pin_id);
            if(start_pin && end_pin) {
                if(start_pin->kind != end_pin->kind) {
                    this->links.emplace_back(this->GetNextId(), start_pin_id, end_pin_id);
                }
            }
            
        }
    }
    ed::EndCreate();


    ed::Suspend();

    if(ed::ShowBackgroundContextMenu()) {
        ImGui::OpenPopup("Create New Node");
    }

    ed::Resume();

    ed::Suspend();
    ImVec2 mouse_pos = ImGui::GetMousePos();
    if(ImGui::BeginPopup("Create New Node")) {
        if(ImGui::MenuItem("Create")) {
            Node* node = SpawnFunctionNode();
            ed::SetNodePosition(node->id, ed::ScreenToCanvas(mouse_pos));
        }
        ImGui::EndPopup();
    }
    ed::Resume();


    BuildNodes();

    ed::End();
}

Node* NodeEditor::SpawnFunctionNode() {
    this->nodes.emplace_back(GetNextId(), "function", ImColor(255, 128, 128));
    this->nodes.back().type = NodeType::Function;
    this->nodes.back().outputs.emplace_back(GetNextId(), "", PinType::Flow);
    BuildNode(&this->nodes.back());
    return &this->nodes.back();
}
Node* NodeEditor::SpawnPlotNode() {
    this->nodes.emplace_back(GetNextId(), "plot", ImColor(255, 255, 255));
    this->nodes.back().type = NodeType::Plot;
    this->nodes.back().inputs.emplace_back(GetNextId(), "", PinType::Float);
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
void NodeEditor::BuildNodes() {
    for(Node& node : this->nodes) {
        this->BuildNode(&node);
    }
}
uint32_t NodeEditor::GetNextId() {
    return this->id_counter++;
}
bool NodeEditor::IsConnected(ed::PinId pin_id) const {
    for(const Link& link : this->links) {
        if(link.start_pin_id == pin_id || link.end_pin_id == pin_id) {
            return true;
        }
    }
    return false;
}
Pin* NodeEditor::GetPin(ed::PinId pin_id) {
    for(Node& node : this->nodes) {
        for(Pin& pin : node.inputs) {
            if(pin.id == pin_id) {
                return &pin;
            }
        }
        for(Pin& pin : node.outputs) {
            if(pin.id == pin_id) {
                return &pin;
            }
        }
    }
    return nullptr;
}
Node* NodeEditor::GetConnectedNode(ed::PinId pin_id) {
    for(const Link& link : this->links) {
        if(link.start_pin_id == pin_id || link.end_pin_id == pin_id) {
            if(link.start_pin_id == pin_id) {
                return GetPin(link.end_pin_id)->node;
            }
            else {
                return GetPin(link.start_pin_id)->node;
            }
        }
    }
    return nullptr;

}

