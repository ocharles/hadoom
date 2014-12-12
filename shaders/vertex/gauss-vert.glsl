// http://xissburg.com/faster-gaussian-blur-in-glsl/
#version 130

varying vec2 v_texCoord;
varying vec2 v_blurTexCoords[4];

float pixel = 1.0f / 256;

void main()
{
    v_texCoord = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    v_blurTexCoords[ 0] = v_texCoord + vec2(0, -2 * pixel);
    v_blurTexCoords[ 1] = v_texCoord + vec2(0, -1 * pixel);
    v_blurTexCoords[ 2] = v_texCoord + vec2(0,  1 * pixel);
    v_blurTexCoords[ 3] = v_texCoord + vec2(0,  2 * pixel);
    gl_Position = vec4(v_texCoord * 2 - 1, 0.0f, 1.0f);
}
