#version 330 core

layout(location = 0) out vec4 o;

uniform sampler2D src;

in vec2 texCoord;
in vec2 offsetTexCoord;

void main ()
{
  o = textureLod(src, texCoord, 0) - textureLod(src, vec2(0, 0), 9);
}
