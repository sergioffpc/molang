#version 300 es

layout(location = 0) in vec2 v_vertex;

void main()
{
    gl_Position = vec4(v_vertex, 0.0, 1.0);
}
