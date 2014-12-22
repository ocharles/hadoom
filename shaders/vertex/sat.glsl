#version 130

uniform vec2 basis;
uniform float pixelSize;
uniform int jump;

out vec2 texCoord;
out vec2 offsetTexCoord;

void main ()
{
    texCoord = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    offsetTexCoord = texCoord + (basis * pixelSize * jump);
    gl_Position = vec4(texCoord * 2 - 1, 0.0f, 1.0f);
}
