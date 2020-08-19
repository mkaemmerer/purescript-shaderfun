'use strict'

const indent = (line, indentLevel) => `${' '.repeat(indentLevel)}${line}`

const indentLines = (lines, indentLevel) =>
  lines
    .split('\n')
    .map((line) => indent(line, indentLevel))
    .join('\n')


const initShader = (gl, { vertexShaderSource, fragmentShaderSource }) => {
  const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vertexShaderSource)
  const fragmentShader = loadShader(
    gl,
    gl.FRAGMENT_SHADER,
    fragmentShaderSource
  )

  // Create the shader program
  const shaderProgram = gl.createProgram()
  gl.attachShader(shaderProgram, vertexShader)
  gl.attachShader(shaderProgram, fragmentShader)
  gl.linkProgram(shaderProgram)

  // If creating the shader program failed, alert
  if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
    throw new Error(
      'Unable to initialize the shader program: ' +
        gl.getProgramInfoLog(shaderProgram)
    )
  }

  return shaderProgram
}

const loadShader = (gl, type, source) => {
  const shader = gl.createShader(type)

  // Send the source to the shader object
  gl.shaderSource(shader, source)

  // Compile the shader program
  gl.compileShader(shader)

  // See if it compiled successfully
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const err = new Error(
      'An error occurred compiling the shaders: ' + gl.getShaderInfoLog(shader)
    )
    gl.deleteShader(shader)
    throw err
  }

  return shader
}

const compileShader = (gl, source) => {
  const vertexShaderSource = `
    precision mediump float;
    attribute vec2 position;
    void main () {
      gl_Position = vec4(position, 0, 1);
    }
  `
  const fragmentShaderSource = `
    precision mediump float;
    uniform float width, height;
    
    float saturate(float v) {
      return clamp(v, 0., 1.);
    }

    vec3 getColor(vec2 p) {
      ${indentLines(source, 6).trim()}
    }

    void main () {
      vec2 resolution = vec2(width, height);
      vec2 p = gl_FragCoord.xy - resolution / 2.;
      vec3 res = getColor(p);
      gl_FragColor = vec4(res.rgb, 1.);
    }
  `

  return initShader(gl, { vertexShaderSource, fragmentShaderSource })
}

const drawShader = (gl, shaderProgram) => () => {
  const canvas = gl.canvas
  canvas.width = canvas.clientWidth * devicePixelRatio
  canvas.height = canvas.clientHeight * devicePixelRatio

  // Prepare viewport
  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height)
  gl.clearColor(0, 0, 0, 0)
  gl.clear(gl.COLOR_BUFFER_BIT)

  // Setup shader
  gl.useProgram(shaderProgram)

  // Setup Uniforms
  const widthLocation = gl.getUniformLocation(shaderProgram, 'width')
  const heightLocation = gl.getUniformLocation(shaderProgram, 'height')

  // Setup Quad
  const quadLocation = 'position'

  const positionBuffer = gl.createBuffer()
  const positionAttributeLocation = gl.getAttribLocation(
    shaderProgram,
    quadLocation
  )

  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer)
  gl.bufferData(
    gl.ARRAY_BUFFER,
    new Float32Array([-1, -1, -1, 1, 1, -1, 1, 1]),
    gl.STATIC_DRAW
  )
  gl.bindBuffer(gl.ARRAY_BUFFER, null)

  // Set Uniforms
  gl.uniform1f(widthLocation, gl.canvas.width)
  gl.uniform1f(heightLocation, gl.canvas.height)

  // Draw Quad
  gl.enableVertexAttribArray(positionAttributeLocation)
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer)

  // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
  const size = 2 // 2 components per iteration
  const type = gl.FLOAT // the data is 32bit floats
  const normalize = false // don't normalize the data
  const stride = 0 // 0 = move forward size * sizeof(type) each iteration to get the next position
  const positionOffset = 0 // start at the beginning of the buffer
  gl.vertexAttribPointer(
    positionAttributeLocation,
    size,
    type,
    normalize,
    stride,
    positionOffset
  )

  const primitiveType = gl.TRIANGLE_STRIP
  const vertexOffset = 0
  const vertexCount = 4
  gl.drawArrays(primitiveType, vertexOffset, vertexCount)
}

const getContext = canvas => canvas.getContext('webgl')

exports.getContextImpl = getContext
exports.compileShaderImpl = compileShader
exports.drawShaderImpl = drawShader