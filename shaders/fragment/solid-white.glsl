#version 400

subroutine float lightRoutine();

out vec4 fragColor;

in vec2 texCoord;
in vec4 shadowCoords;
in vec3 lightDirEyeSpace;
in vec3 lightEyeDirTangentSpace;
in vec3 lightEyeDirEyeSpace;

uniform sampler2D tex;
uniform sampler2DShadow depthMap;
uniform sampler2D nmap;
subroutine uniform lightRoutine lightContribution;

struct LightInfo {
  vec3 pos;
  vec3 color;
  vec3 direction;
  float radius;
};
layout(std140) uniform Light {
  LightInfo light;
};

const float minLight = 0.01;

void main(void) {
  vec3 fragToLight = normalize(lightEyeDirTangentSpace);
  vec3 normal = normalize(texture2D(nmap, texCoord).rgb * 2.0 - 1.0);

  float d = length(lightEyeDirEyeSpace);
  float b = 1.0 / (light.radius * light.radius * minLight);
  float att = 1 + b * d * d;
  float energy = clamp(dot(normal, fragToLight), 0, 1) / att;

  vec3 diffuse = texture2D(tex, texCoord).rgb;

  fragColor = vec4(lightContribution() * diffuse * light.color * energy, 1);
}


////////////////////////////////////////////////////////////////////////////////


subroutine (lightRoutine)

float spotlight() {
  float visibility = texture(depthMap, shadowCoords.xyz / shadowCoords.w);
  float spot = (dot(lightDirEyeSpace, -normalize(lightEyeDirEyeSpace)) > 0.5) ? 1 : 0;
  return spot * visibility;
}


////////////////////////////////////////////////////////////////////////////////

subroutine (lightRoutine)

float omni() {
  return 1;
}
