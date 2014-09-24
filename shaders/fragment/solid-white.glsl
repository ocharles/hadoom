#version 330

out vec4 fragColor;

in vec3 norm;
in vec3 worldPos;
in vec2 texCoord;

struct LightInfo {
  vec3 lightPos;
  vec3 lightColor;
  float radius;
};

uniform sampler2D tex;

layout(std140) uniform Light {
	LightInfo Lights[2];
};

const float minLight = 0.01;

void main(void) {
  for(int i = 0; i < 2; i++) {
    LightInfo light = Lights[i];
    vec3 dir = light.lightPos - worldPos;
    float d = length(dir);
    vec3 l = dir / d;
    float b = 1.0 / (light.radius * light.radius * minLight);
    float att = 1 + b * d * d;
    fragColor += vec4(texture2D(tex, texCoord).rgb * light.lightColor * clamp(dot(norm, l), 0, 1) / att, 1);
  }
}
