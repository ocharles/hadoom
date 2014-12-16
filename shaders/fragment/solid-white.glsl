#version 400

subroutine float lightRoutine();

out vec4 fragColor;

in vec2 texCoord;
in vec4 shadowCoords;
in vec3 lightDirEyeSpace;
in vec3 lightEyeDirTangentSpace;
in vec3 lightEyeDirEyeSpace;
in vec3 wp;

uniform sampler2D tex;
uniform usampler2D depthMap;
uniform sampler2D nmap;
subroutine uniform lightRoutine lightContribution;

struct LightInfo {
  vec3 pos;
  vec3 color;
  float radius;
};

struct SpotlightInfo {
  vec3 direction;
  float cosConeRadius;
  float cosPenumbraRadius;
};

layout(std140) uniform Light {
  LightInfo light;
  SpotlightInfo spotlightParams;
};

const float minLight = 0.01;

void main(void) {
  vec3 fragToLight = normalize(lightEyeDirTangentSpace);
  vec3 normal = vec3(0, 0, 1); // normalize(texture2D(nmap, texCoord).rgb * 2.0 - 1.0);

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
  vec3 shadowDiv = shadowCoords.xyz / shadowCoords.w;

  float near = 1.0f;
  float far = 20.0f;
  float c = 88.0f;
  float scale = 1073741824;

  float pixel = 1.0f / 2048.0f;
  uint lightDepthInt = texture(depthMap, shadowDiv.xy).r;

  float ourDepth = (length(wp) - near) / (far - near);

  float num = 0;
  float denom = 0;
  float cz = exp(-c * ourDepth);

  float w = 15;

  int bk = int(round(ourDepth * w));

  for(int y = -bk; y <= bk; y++) {
    for(int x = -bk; x <= bk; x++) {
      float d = float(texture(depthMap, shadowDiv.xy + 1 * vec2(pixel * x, pixel * y)).r / scale);
      num += clamp(cz * exp(c * d), 0, 1) * d;
      denom += clamp(cz * exp(c * d), 0, 1);
    }
  }

  float zavg = denom > 0 ? num / denom : 1;

  float visibility = 0;
  int k = int(round(zavg * w));

  for(int y = -k; y <= k; y++) {
    for(int x = -k; x <= k; x++) {
      float d = float(texture(depthMap, shadowDiv.xy + 1 * vec2(pixel * x, pixel * y)).r / scale);
      visibility += clamp(exp(-c * ourDepth) * exp(c * d), 0, 1);
    }
  }

  visibility /= k > 0 ? (k * 2 + 1) * (k * 2 + 1) : 1;

  float theta = dot(lightDirEyeSpace, -normalize(lightEyeDirEyeSpace));

  return clamp(visibility, 0, 1) * smoothstep(0, 1, (theta - spotlightParams.cosConeRadius) / spotlightParams.cosPenumbraRadius);
}

////////////////////////////////////////////////////////////////////////////////

subroutine (lightRoutine)

float omni() {
  return 1;
}
