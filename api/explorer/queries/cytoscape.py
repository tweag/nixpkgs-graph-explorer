"""Cytoscape.js data model

This module implements the Cytoscape.js data model in Python. This is useful for
constructing cytoscape-compatible data, for example a JSON object which can be read by
Cytoscape.js.

Note that this data model aims to be compatible with Cytoscape.js ^3.19

The basis for this module was programmatically generated using datamodel-code-generator
(version 0.17.1) from the JSON Schema available at:
    https://github.com/AZaitzeff/cytoscape_js_schema
    (rev: 28aa3e15afb9870a0303dae87fe15c1a58361647)

"""

from __future__ import annotations

from enum import Enum
from typing import Annotated, Any, Dict, List, Optional, Union

from pydantic import BaseModel, Extra, Field, confloat

PositiveFloat = Annotated[float, confloat(ge=0.0)]
ClosedIntervalOneFloat = Annotated[float, confloat(ge=-1.0, le=1.0)]
UnitIntervalFloat = Annotated[float, confloat(ge=0.0, le=1.0)]


class Group(Enum):
    nodes = "nodes"
    edges = "edges"


class EdgeData(BaseModel):
    id: Optional[str] = Field(
        default=None,
        description=(
            "optional (string) id for each element, assigned automatically on undefined"
        ),
    )
    source: str = Field(
        description="the source node id (edge comes from this node)",
    )
    target: str = Field(description="the target node id (edge goes to this node)")
    # Note: cytoscape.js accepts an arbitrary number of other properties as well,
    # but we have not implemented them in the schema here


class NodeData(BaseModel):
    id: Optional[str] = Field(
        default=None,
        description=(
            "optional (string) id for each element, assigned automatically on undefined"
        ),
    )
    parent: Optional[str] = Field(
        default=None,
        description="indicates the compound node parent id; not defined => no parent",
    )
    # Note: cytoscape.js accepts an arbitrary number of other properties as well,
    # but we have not implemented them in the schema here


class Position(BaseModel):
    x: Optional[float] = None
    y: Optional[float] = None


class RenderedPosition(BaseModel):
    x: Optional[float] = None
    y: Optional[float] = None


class Element(BaseModel):
    data: Union[NodeData, EdgeData] = Field(None, description="element data")
    group: Optional[Group] = Field(
        default=None,
        description=(
            "nodes' for a node, 'edges' for an edge. NB the group field can be"
            " automatically inferred for you but specifying it gives you nice debug"
            " messages if you mis-init elements"
        ),
    )
    position: Optional[Position] = Field(
        default=None, description="the model position of the node"
    )
    renderedPosition: Optional[RenderedPosition] = Field(
        default=None,
        description="can alternatively specify position in rendered on-screen pixels",
    )
    selected: Optional[bool] = Field(default=False, description="")
    selectable: Optional[bool] = Field(
        default=True, description="whether the selection state is mutable"
    )
    locked: Optional[bool] = Field(
        default=False, description="when locked a node's position is immutable"
    )
    grabbable: Optional[bool] = Field(
        default=True,
        description="whether the node can be grabbed and moved by the user",
    )
    pannable: Optional[bool] = Field(
        default=False,
        description="whether dragging the node causes panning instead of grabbing",
    )
    classes: Optional[List[str]] = Field(
        default=None, description="an array of class names that the element has"
    )


class NodeDefinition(Element):
    data: NodeData


class EdgeDefinition(Element):
    data: EdgeData


class ElementsDefinition(BaseModel):
    nodes: list[NodeDefinition]
    edges: list[EdgeDefinition]


class Name(Enum):
    null = "null"
    random = "random"
    preset = "preset"
    grid = "grid"
    circle = "circle"
    concentric = "concentric"
    breadthfirst = "breadthfirst"
    cose = "cose"


class Layout(BaseModel):
    name: Name = Field(..., description="Type of layout")
    fit: Optional[bool] = Field(default=True, description="whether to fit to viewport")
    padding: Optional[float] = Field(default=30, description="fit padding")
    boundingBox: Optional[Dict[str, Any]] = Field(
        default=None,
        description="constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }",
    )
    animate: Optional[bool] = Field(
        default=None, description="whether to transition the node positions"
    )
    animationDuration: Optional[float] = Field(
        default=500, description="duration of animation in ms if enabled"
    )
    positions: Optional[Dict[str, Any]] = Field(
        default=None,
        description=(
            "layout = preset, map of (node id) => (position obj); or function(node){"
            " return somPos; }"
        ),
    )
    avoidOverlap: Optional[bool] = Field(
        default=True,
        description=(
            "When layout is grid, circle, concentric, breadthfirst. prevents node"
            " overlap, may overflow boundingBox if not enough space"
        ),
    )
    avoidOverlapPadding: Optional[float] = Field(
        default=10,
        description=(
            "When layout is grid. Extra spacing around nodes when avoidOverlap: true"
        ),
    )
    nodeDimensionsIncludeLabels: Optional[bool] = Field(
        default=False,
        description=(
            "Excludes the label when calculating node bounding boxes for the layout"
            " algorithm"
        ),
    )
    spacingFactor: Optional[PositiveFloat] = Field(
        default=1,
        description=(
            "When layout is grid, circle, breadthfirst. grid, circle: Applies a"
            " multiplicative factor (>0) to expand or compress the overall area that"
            " the nodes take up.breathfirst: positive spacing factor, larger => more"
            " space between nodes (N.B. n/a if causes overlap)"
        ),
    )
    condense: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is grid. uses all available space on false, uses minimal space"
            " on true"
        ),
    )
    rows: Optional[int] = Field(
        default=None, description="When layout is grid. force num of rows in the grid"
    )
    cols: Optional[int] = Field(
        default=None,
        description="When layout is grid. force num of columns in the grid",
    )
    radius: Optional[float] = Field(
        default=None, description="When layout is circle. the radius of the circle"
    )
    startAngle: Optional[float] = Field(
        default=4.712,
        description="When layout is circle, concentric. where nodes start in radians",
    )
    sweep: Optional[float] = Field(
        default=None,
        description=(
            "When layout is circle, concentric. how many radians should be between the"
            " first and last node (defaults to full circle)"
        ),
    )
    clockwise: Optional[bool] = Field(
        default=None,
        description=(
            "When layout is circle, concentric. whether the layout should go clockwise"
            " (true) or counterclockwise/anticlockwise (false)"
        ),
    )
    equidistant: Optional[bool] = Field(
        default=None,
        description=(
            "When layout is concentric. whether levels have an equal radial distance"
            " betwen them, may cause bounding box overflow"
        ),
    )
    minNodeSpacing: Optional[float] = Field(
        default=None,
        description=(
            "When layout is concentric. min spacing between outside of nodes (used for"
            " radius adjustment)"
        ),
    )
    directed: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is breadthfirst. whether the tree is directed downwards (or"
            " edges can point in any direction if false)"
        ),
    )
    circle: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is breadthfirst. put depths in concentric circles if true, put"
            " depths top down if false"
        ),
    )
    grid: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is breadthfirst. whether to create an even grid into which the"
            " DAG is placed (circle:false only)"
        ),
    )
    roots: Optional[str] = Field(
        default=None,
        description=(
            "When layout is breadthfirst. Format of selector, the roots of the trees"
        ),
    )
    maximal: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is breadthfirst. whether to shift nodes down their natural BFS"
            " depths in order to avoid upwards edges (DAGS only)"
        ),
    )
    randomize: Optional[bool] = Field(
        default=False,
        description=(
            "When layout is cose. Randomize the initial positions of the nodes (true)"
            " or use existing positions (false)"
        ),
    )
    componentSpacing: Optional[float] = Field(
        default=40,
        description=(
            "When layout is cose. Extra spacing between components in non-compound"
            " graphs"
        ),
    )
    nodeOverlap: Optional[float] = Field(
        default=4,
        description="When layout is cose. Node repulsion (overlapping) multiplier",
    )
    nestingFactor: Optional[float] = Field(
        default=1.2,
        description=(
            "When layout is cose. Nesting factor (multiplier) to compute ideal edge"
            " length for nested edges"
        ),
    )
    gravity: Optional[float] = Field(
        default=1, description="When layout is cose. Gravity force (constant)"
    )
    numIter: Optional[float] = Field(
        default=1000,
        description="When layout is cose. Maximum number of iterations to perform",
    )
    initialTemp: Optional[float] = Field(
        default=1000,
        description=(
            "When layout is cose. Initial temperature (maximum node displacement)"
        ),
    )
    coolingFactor: Optional[float] = Field(
        default=0.99,
        description=(
            "When layout is cose. Cooling factor (how the temperature is reduced"
            " between consecutive iterations)"
        ),
    )
    minTemp: Optional[float] = Field(
        default=1.0,
        description=(
            "When layout is cose. Lower temperature threshold (below this point the"
            " layout will end)"
        ),
    )


class Shape(Enum):
    ellipse = "ellipse"
    triangle = "triangle"
    round_triangle = "round-triangle"
    rectangle = "rectangle"
    round_rectangle = "round-rectangle"
    bottom_round_rectangle = "bottom-round-rectangle"
    cut_rectangle = "cut-rectangle"
    barrel = "barrel"
    rhomboid = "rhomboid"
    diamond = "diamond"
    round_diamond = "round-diamond"
    pentagon = "pentagon"
    round_pentagon = "round-pentagon"
    hexagon = "hexagon"
    round_hexagon = "round-hexagon"
    concave_hexagon = "concave-hexagon"
    heptagon = "heptagon"
    round_heptagon = "round-heptagon"
    octagon = "octagon"
    round_octagon = "round-octagon"
    star = "star"
    tag = "tag"
    round_tag = "round-tag"
    vee = "vee"


class BorderStyle(Enum):
    solid = "solid"
    dotted = "dotted"
    dashed = "dashed"
    double = "double"


class CurveStyle(Enum):
    straight = "straight"
    haystack = "haystack"
    bezier = "bezier"
    unbundled_bezier = "unbundled-bezier"
    segments = "segments"
    taxi = "taxi"


class LineStyle(Enum):
    solid = "solid"
    dotted = "dotted"
    dashed = "dashed"


class LineCap(Enum):
    butt = "butt"
    round = "round"
    square = "square"


class TargetArrowShape(Enum):
    triangle = "triangle"
    triangle_tee = "triangle-tee"
    circle_triangle = "circle-triangle"
    triangle_cross = "triangle-cross"
    triangle_backcurve = "triangle-backcurve"
    vee = "vee"
    tee = "tee"
    square = "square"
    circle = "circle"
    diamond = "diamond"
    chevron = "chevron"
    none = "none"


class TargetArrowFill(Enum):
    filled = "filled"
    hollow = "hollow"


class MidTargetArrowShape(Enum):
    triangle = "triangle"
    triangle_tee = "triangle-tee"
    circle_triangle = "circle-triangle"
    triangle_cross = "triangle-cross"
    triangle_backcurve = "triangle-backcurve"
    vee = "vee"
    tee = "tee"
    square = "square"
    circle = "circle"
    diamond = "diamond"
    chevron = "chevron"
    none = "none"


class MidTargetArrowFill(Enum):
    filled = "filled"
    hollow = "hollow"


class TextTransform(Enum):
    none = "none"
    uppercase = "uppercase"
    lowercase = "lowercase"


class TextHalign(Enum):
    left = "left"
    center = "center"
    right = "right"


class TextValign(Enum):
    top = "top"
    center = "center"
    bottom = "bottom"


class Ghost(Enum):
    yes = "yes"
    no = "no"


class Style(BaseModel):
    width: Optional[float] = Field(
        default=None,
        description="The width of the node’s body or the width of an edge’s line.",
    )
    height: Optional[float] = Field(
        default=None, description="The height of the node’s body"
    )
    shape: Optional[Shape] = Field(
        default=None,
        description=(
            "The shape of the node’s body. Note that each shape fits within the"
            " specified width and height, and so you may have to adjust width and"
            " height if you desire an equilateral shape (i.e. width !== height for"
            " several equilateral shapes)"
        ),
    )
    background_color: Optional[str] = Field(
        default=None,
        alias="background-color",
        description=(
            "The colour of the node’s body. Colours may be specified by name (e.g."
            " red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    background_blacken: Optional[ClosedIntervalOneFloat] = Field(
        default=None,
        alias="background-blacken",
        description=(
            "Blackens the node’s body for values from 0 to 1; whitens the node’s body"
            " for values from 0 to -1."
        ),
    )
    background_opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        alias="background-opacity",
        description="The opacity level of the node’s background colour",
    )
    border_width: Optional[PositiveFloat] = Field(
        default=None, alias="border-width", description="The size of the node’s border."
    )
    border_style: Optional[BorderStyle] = Field(
        default=None, alias="border-style", description="The style of the node’s border"
    )
    border_color: Optional[str] = Field(
        default=None,
        alias="border-color",
        description=(
            "The colour of the node’s border. Colours may be specified by name (e.g."
            " red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    border_opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        alias="border-opacity",
        description="The opacity of the node’s border",
    )
    padding: Optional[PositiveFloat] = Field(
        default=None, description="The amount of padding around all sides of the node."
    )
    curve_style: Optional[CurveStyle] = Field(
        "straight",
        alias="curve-style",
        description=(
            "The curving method used to separate two or more edges between two nodes;"
            " may be haystack (very fast, bundled straight edges for which loops and"
            " compounds are unsupported), straight (straight edges with all arrows"
            " supported), bezier (bundled curved edges), unbundled-bezier (curved edges"
            " for use with manual control points), segments (a series of straight"
            " lines), taxi (right-angled lines, hierarchically bundled). Note that"
            " haystack edges work best with ellipse, rectangle, or similar nodes."
            " Smaller node shapes, like triangle, will not be as aesthetically"
            " pleasing. Also note that edge endpoint arrows are unsupported for"
            " haystack edges."
        ),
    )
    line_color: Optional[str] = Field(
        default=None,
        alias="line-color",
        description=(
            "The colour of the edge’s line. Colours may be specified by name (e.g."
            " red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    line_style: Optional[LineStyle] = Field(
        default=None, alias="line-style", description="The style of the edge’s line."
    )
    line_cap: Optional[LineCap] = Field(
        default="butt",
        alias="line-cap",
        description=(
            "The cap style of the edge’s line; may be butt (default), round, or square."
            " The cap may or may not be visible, depending on the shape of the node and"
            " the relative size of the node and edge. Caps other than butt extend"
            " beyond the specified endpoint of the edge."
        ),
    )
    line_opacity: Optional[UnitIntervalFloat] = Field(
        default=1,
        alias="line-opacity",
        description=(
            "The opacity of the edge’s line and arrow. Useful if you wish to have a"
            " separate opacity for the edge label versus the edge line. Note that the"
            " opacity value of the edge element affects the effective opacity of its"
            " line and label subcomponents."
        ),
    )
    target_arrow_color: Optional[str] = Field(
        default=None,
        alias="target-arrow-color",
        description=(
            "The colour of the edge’s source arrow. Colours may be specified by name"
            " (e.g. red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL"
            " (e.g. hsl(0, 100%, 50%))."
        ),
    )
    target_arrow_shape: Optional[TargetArrowShape] = Field(
        default=None,
        alias="target-arrow-shape",
        description="The shape of the edge’s source arrow",
    )
    target_arrow_fill: Optional[TargetArrowFill] = Field(
        default=None,
        alias="target-arrow-fill",
        description="The fill state of the edge’s source arrow",
    )
    mid_target_arrow_color: Optional[str] = Field(
        default=None,
        alias="mid-target-arrow-color",
        description=(
            "The colour of the edge’s source arrow. Colours may be specified by name"
            " (e.g. red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL"
            " (e.g. hsl(0, 100%, 50%))."
        ),
    )
    mid_target_arrow_shape: Optional[MidTargetArrowShape] = Field(
        default=None,
        alias="mid-target-arrow-shape",
        description="The shape of the edge’s source arrow",
    )
    mid_target_arrow_fill: Optional[MidTargetArrowFill] = Field(
        default=None,
        alias="mid-target-arrow-fill",
        description="The fill state of the edge’s source arrow",
    )
    arrow_scale: Optional[PositiveFloat] = Field(
        default=None, alias="arrow-scale", description="Scaling for the arrow size."
    )
    opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        description=(
            "The opacity of the element. See https://js.cytoscape.org/#style/visibility"
        ),
    )
    z_index: Optional[int] = Field(
        default=None,
        alias="z-index",
        description=(
            "An integer value that affects the relative draw order of elements. In"
            " general, an element with a higher z-index will be drawn on top of an"
            " element with a lower z-index. Note that edges are under nodes despite"
            " z-index."
        ),
    )
    label: Optional[str] = Field(
        default=None,
        description=(
            "The text to display for an element’s label. Can give a path, e.g. data(id)"
            " will label with the elements id"
        ),
    )
    source_label: Optional[str] = Field(
        default=None,
        alias="source-label",
        description=(
            "The text to display for an edge’s source label. Can give a path, e.g."
            " data(id) will label with the elements id"
        ),
    )
    target_label: Optional[str] = Field(
        default=None,
        alias="target-label",
        description=(
            "The text to display for an edge’s target label. Can give a path, e.g."
            " data(id) will label with the elements id"
        ),
    )
    color: Optional[str] = Field(
        default=None,
        description=(
            "The color of the element's label. Colours may be specified by name (e.g."
            " red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    text_opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        alias="text-opacity",
        description="The opacity of the label text, including its outline.",
    )
    font_family: Optional[str] = Field(
        default=None,
        alias="font-family",
        description="A comma-separated list of font names to use on the label text.",
    )
    font_size: Optional[str] = Field(
        default=None, alias="font-size", description="The size of the label text."
    )
    font_style: Optional[str] = Field(
        default=None,
        alias="font-style",
        description="A CSS font style to be applied to the label text.",
    )
    font_weight: Optional[str] = Field(
        default=None,
        alias="font-weight",
        description="A CSS font weight to be applied to the label text.",
    )
    text_transform: Optional[TextTransform] = Field(
        default=None,
        alias="text-transform",
        description="A transformation to apply to the label text",
    )
    text_halign: Optional[TextHalign] = Field(
        default=None,
        alias="text-halign",
        description="The horizontal alignment of a node’s label",
    )
    text_valign: Optional[TextValign] = Field(
        default=None,
        alias="text-valign",
        description="The vertical alignment of a node’s label",
    )
    ghost: Optional[Ghost] = Field(
        default="no",
        description=(
            "Whether to use the ghost effect, a semitransparent duplicate of the"
            " element drawn at an offset."
        ),
    )
    active_bg_color: Optional[str] = Field(
        default=None,
        alias="active-bg-color",
        description=(
            "The colour of the indicator shown when the background is grabbed by the"
            " user. Selector needs to be *core*. Colours may be specified by name (e.g."
            " red), hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    active_bg_opacity: Optional[str] = Field(
        default=None,
        alias="active-bg-opacity",
        description=(
            " The opacity of the active background indicator. Selector needs to be"
            " *core*."
        ),
    )
    active_bg_size: Optional[str] = Field(
        default=None,
        alias="active-bg-size",
        description=(
            " The opacity of the active background indicator. Selector needs to be"
            " *core*."
        ),
    )
    selection_box_color: Optional[str] = Field(
        default=None,
        alias="selection-box-color",
        description=(
            "The background colour of the selection box used for drag selection."
            " Selector needs to be *core*. Colours may be specified by name (e.g. red),"
            " hex (e.g. #ff0000 or #f00), RGB (e.g. rgb(255, 0, 0)), or HSL (e.g."
            " hsl(0, 100%, 50%))."
        ),
    )
    selection_box_border_width: Optional[float] = Field(
        default=None,
        alias="selection-box-border-width",
        description=(
            "The size of the border on the selection box. Selector needs to be *core*"
        ),
    )
    selection_box_opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        alias="selection-box-opacity",
        description="The opacity of the selection box.  Selector needs to be *core*",
    )
    outside_texture_bg_color: Optional[str] = Field(
        default=None,
        alias="outside-texture-bg-color",
        description=(
            "The colour of the area outside the viewport texture when"
            " initOptions.textureOnViewport === true.  Selector needs to be *core*."
            " Colours may be specified by name (e.g. red), hex (e.g. #ff0000 or #f00),"
            " RGB (e.g. rgb(255, 0, 0)), or HSL (e.g. hsl(0, 100%, 50%))."
        ),
    )
    outside_texture_bg_opacity: Optional[UnitIntervalFloat] = Field(
        default=None,
        alias="outside-texture-bg-opacity",
        description=(
            "The opacity of the area outside the viewport texture. Selector needs to be"
            " *core*"
        ),
    )


class StyleItem(BaseModel):
    selector: str = Field(
        ...,
        description=(
            "Where to apply the style element, common inputs are *node* or *edge*, Also"
            " takes in css selector, e.g. *.foo* to apply to all edges and nodes of"
            ' class *foo*, #foo (or [id="foo"]) for node or edge of with id foo. See'
            " https://js.cytoscape.org/#selectors/notes-amp-caveats"
        ),
    )
    style: Style = Field(..., description="Specify the styles")


class Pan(BaseModel):
    class Config:
        extra = Extra.forbid

    x: Optional[float] = 0
    y: Optional[float] = 0


class SelectionType(Enum):
    single = "single"
    additive = "additive"


class CytoscapeJs(BaseModel):
    elements: Optional[ElementsDefinition | List[Element]] = Field(
        default=None,
        description="The graph's elements",
    )
    layout: Optional[Layout] = Field(
        default=None,
        description="Placement of the nodes see https://js.cytoscape.org/#layouts",
    )
    style: Optional[List[StyleItem]] = Field(
        default=None,
        description=(
            "Style applied to a group, e.g. all nodes, all edges, nodes in a certain"
            " class or a single node, etc. Each element is CSS-like, See"
            " https://js.cytoscape.org/#style"
        ),
    )
    zoom: Optional[float] = Field(
        default="1",
        description=(
            "The initial zoom level of the graph. Make sure to disable viewport"
            " manipulation options, such as fit, in your layout so that it is not"
            " overridden when the layout is applied. You can set options.minZoom and"
            " options.maxZoom to set restrictions on the zoom level"
        ),
    )
    pan: Optional[Pan] = Field(
        default=None,
        description=(
            "The initial panning position of the graph. Make sure to disable viewport"
            " manipulation options, such as fit, in your layout so that it is not"
            " overridden when the layout is applied."
        ),
    )
    minZoom: Optional[float] = Field(
        default=None,
        description=(
            "A minimum bound on the zoom level of the graph. The viewport cannot be"
            " scaled smaller than this zoom level."
        ),
    )
    maxZoom: Optional[float] = Field(
        default=None,
        description=(
            "A maximum bound on the zoom level of the graph. The viewport cannot be"
            " scaled larger than this zoom level."
        ),
    )
    zoomingEnabled: Optional[bool] = Field(
        default=True,
        description=(
            "Whether zooming the graph is enabled, both by user events and"
            " programmatically."
        ),
    )
    userZoomingEnabled: Optional[bool] = Field(
        default=True,
        description=(
            "Whether user events (e.g. mouse wheel, pinch-to-zoom) are allowed to zoom"
            " the graph. Programmatic changes to zoom are unaffected by this option."
        ),
    )
    panningEnabled: Optional[bool] = Field(
        default=True,
        description=(
            "Whether panning the graph is enabled, both by user events and"
            " programmatically."
        ),
    )
    userPanningEnabled: Optional[bool] = Field(
        default=True,
        description=(
            "Whether user events (e.g. dragging the graph background) are allowed to"
            " pan the graph. Programmatic changes to pan are unaffected by this option."
        ),
    )
    boxSelectionEnabled: Optional[bool] = Field(
        default=True,
        description=(
            "Whether box selection (i.e. drag a box overlay around, and release it to"
            " select) is enabled. If enabled while panning is also enabled, the user"
            " must use a modifier key (shift, alt, control, or command) to use box"
            " selection."
        ),
    )
    selectionType: Optional[SelectionType] = Field(
        default="single",
        description=(
            "A string indicating the selection behavior from user input. For"
            " 'additive', a new selection made by the user adds to the set of currently"
            " selected elements. For 'single', a new selection made by the user becomes"
            " the entire set of currently selected elements (i.e. the previous elements"
            " are unselected)."
        ),
    )
    touchTapThreshold: Optional[float] = Field(
        default=8,
        description=(
            "A non-negative integer that indicates the maximum allowable distance that"
            " a user may move during a tap gesture on touch devices. This makes tapping"
            " easier for users. These values have sane defaults, so it is not advised"
            " to change these options unless you have very good reason for doing so."
            " Large values will almost certainly have undesirable consequences."
        ),
    )
    desktopTapThreshold: Optional[float] = Field(
        default=4,
        description=(
            "A non-negative integer that indicates the maximum allowable distance that"
            " a user may move during a tap gesture on desktop devices. This makes"
            " tapping easier for users. These values have sane defaults, so it is not"
            " advised to change these options unless you have very good reason for"
            " doing so. Large values will almost certainly have undesirable"
            " consequences."
        ),
    )
    autoungrabify: Optional[bool] = Field(
        default=False,
        description=(
            "Whether nodes should be ungrabified (not grabbable by user) by default (if"
            " true, overrides individual node state)"
        ),
    )
    autolock: Optional[bool] = Field(
        default=False,
        description=(
            "Whether nodes should be locked (not draggable at all) by default (if true,"
            " overrides individual node state)."
        ),
    )
    autounselectify: Optional[bool] = Field(
        default=False,
        description=(
            "Whether nodes should be unselectified (immutable selection state) by"
            " default (if true, overrides individual element state)."
        ),
    )
    headless: Optional[bool] = Field(
        default=False,
        description=(
            "A convenience option that initializes the instance to run headlessly. You"
            " do not need to set this in environments that are implicitly headless"
            " (e.g. Node.js). However, it is handy to set headless: true if you want a"
            " headless instance in a browser."
        ),
    )
    styleEnabled: Optional[bool] = Field(
        default=False,
        description=(
            "A boolean that indicates whether styling should be used. For headless"
            " (i.e. outside the browser) environments, display is not necessary and so"
            " neither is styling necessary — thereby speeding up your code. You can"
            " manually enable styling in headless environments if you require it for a"
            " special case. Note that it does not make sense to disable style if you"
            " plan on rendering the graph. Also note that cy.destroy() must be called"
            " to clean up a style-enabled, headless instance."
        ),
    )
    wheelSensitivity: Optional[float] = Field(
        default=1,
        description=(
            "Changes the scroll wheel sensitivity when zooming. This is a"
            " multiplicative modifier. So, a value between 0 and 1 reduces the"
            " sensitivity (zooms slower), and a value greater than 1 increases the"
            " sensitivity (zooms faster). This option is set to a sane value that works"
            " well for mainstream mice (Apple, Logitech, Microsoft) on Linux, Mac, and"
            " Windows. If the default value seems too fast or too slow on your"
            " particular system, you may have non-default mouse settings in your OS or"
            " a niche mouse. You should not change this value unless your app is meant"
            " to work only on specific hardware. Otherwise, you risk making zooming too"
            " slow or too fast for most users."
        ),
    )
