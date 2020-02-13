namespace fTrafficCore

module Types =

    type Position = { x: float; y: float; z: float }

    type Node =
        | Empty
        | CrossRoad
        | TrafficLight
        | Car

    type Point = { p: Position; t: Node }

    type Road = { points: Point list }

    type RoadNetwork = { roads: Road list }