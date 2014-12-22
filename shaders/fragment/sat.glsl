#version 330 core

layout(location = 0) out vec4 o;

uniform sampler2D src;

in vec2 texCoord;
in vec2 offsetTexCoord;

void main ()
{
  if (offsetTexCoord.x >= 0 && offsetTexCoord.y >= 0 && offsetTexCoord.x <= 1 && offsetTexCoord.y <= 1) {
    o = textureLod(src, texCoord, 0) + textureLod(src, offsetTexCoord, 0);
  } else {
    o = textureLod(src, texCoord, 0);
  }
}
