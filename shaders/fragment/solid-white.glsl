#version 330

out vec4 fragColor;

in vec3 norm;
in vec3 worldPos;
in vec2 texCoord;
in vec4 shadowCoords;

struct LightInfo {
  vec3 lightPos;
  vec3 lightColor;
  float radius;
};

uniform sampler2D tex;
uniform sampler2DShadow shadowTex;

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

    float visibility = texture(shadowTex, vec3(shadowCoords.xyz) / shadowCoords.w);
    if (i > 0) { visibility = 1; }
    vec3 diffuse = texture2D(tex, texCoord).rgb;
    fragColor += vec4(visibility * diffuse * light.lightColor * clamp(dot(norm, l), 0, 1) / att, 1);
  }
}
