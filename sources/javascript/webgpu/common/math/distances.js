import vec3 from './vec3.js';

function lineSegment3Sphere(segmentStart, normalSegmentDir, sphereCenter)
{
    var dirToCenter = vec3.sub(sphereCenter, segmentStart);
    var dot = vec3.dot(dirToCenter, normalSegmentDir);
    var proj = vec3.scale(vec3.copy(normalSegmentDir), dot);
    var closestPoint = vec3.sum(segmentStart, proj);
    return vec3.distance(closestPoint, sphereCenter);
}

export default { lineSegment3Sphere }