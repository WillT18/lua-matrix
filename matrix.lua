local matrix = {}

local function magnitude(x, y, z)
	return math.sqrt(x*x + y*y + z*z)
end

local function normal(x, y, z, s)
	local l = math.sqrt(x*x + y*y + z*z)
	return s*x/l, s*y/l, s*z/l, l
end

local function cross(x, y, z, a, b, c, s)
	local d, e, f = y*c - z*b, z*a - x*c, x*b - y*a
	local l = math.sqrt(d*d + e*e + f*f)
	return s*d/l, s*e/l, s*f/l, l
end

local function m_unpack(t)
	return
		t[1], t[2], t[3], t[4],
		t[5], t[6], t[7], t[8],
		t[9], t[10],t[11],t[12],
		t[13],t[14],t[15],t[16]
end

local meta = {
	__metatable = "fast_mutable_matrix",
	__index = matrix,
	__newindex = function() end,
	__tostring = function(self)
		return string.format("%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f", m_unpack(self))
	end
}

function matrix.new()
	return setmetatable({
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	}, meta)
end

function matrix:getPosition()
	return self[4], self[8], self[12]
end

function matrix:getScale()
	return
		magnitude(self[1], self[5], self[9]),
		magnitude(self[2], self[6], self[10]),
		magnitude(self[3], self[7], self[11])
end

function matrix:getForward()
	return normal(-self[1], -self[5], -self[9], 1)
end

function matrix:getRight()
	return normal(self[2], self[6], self[10], 1)
end

function matrix:getUp()
	return normal(self[3], self[7], self[11], 1)
end

function matrix:reset()
	self[1], self[2], self[3], self[4] = 1, 0, 0, 0
	self[5], self[6], self[7], self[8] = 0, 1, 0, 0
	self[9], self[10],self[11],self[12]= 0, 0, 1, 0
	self[13],self[14],self[15],self[16]= 0, 0, 0, 1
	return self
end

function matrix:position(x, y, z)
	self[4], self[8], self[12] = x, y, z
	return self
end

function matrix:scale(x, y, z)
	self[1], self[5], self[9] = normal(self[1], self[5], self[9], x)
	self[2], self[6], self[10]= normal(self[2], self[6], self[10],y)
	self[3], self[7], self[11]= normal(self[3], self[7], self[11],z)
	return self
end

function matrix:rotation(r, p, y)
	local scale_x = magnitude(self[1], self[5], self[9])
	local scale_y = magnitude(self[2], self[6], self[10])
	local scale_z = magnitude(self[3], self[7], self[11])
	local ca, cb, cc = math.cos(y), math.cos(p), math.cos(r)
	local sa, sb, sc = math.sin(y), math.sin(p), math.sin(r)
	self[1], self[5], self[9] = normal(ca*cb,            sa*cb,           -sb,    scale_x)
	self[2], self[6], self[10]= normal(ca*sb*sc - sa*cc, sa*sb*sc + ca*cc, cb*sc, scale_y)
	self[3], self[7], self[11]= normal(ca*sb*cc + sa*sc, sa*sb*cc - ca*sc, cb*cc, scale_z)
	return self
end

function matrix:target(x, y, z, sx, sy, sz)
	local scale_x = magnitude(self[1], self[5], self[9])
	local scale_y = magnitude(self[2], self[6], self[10])
	local scale_z = magnitude(self[3], self[7], self[11])
	local bx, by, bz, bl = normal(self[4] - x, self[8] - y, self[12] - z, scale_x)
	if (bl > 0) then
		if (sx == nil) then
			sx, sy, sz = 0, 0, 1
		end
		local rx, ry, rz, rl = cross(sx, sy, sz, bx, by, bz, scale_y)
		if (rl == 0) then
			if (sx == 0 and sy == 0) then
				rx, ry, rz = cross(1, 0, 0, bx, by, bz, scale_y)
			else
				rx, ry, rz = cross(0, 0, 1, bx, by, bz, scale_y)
			end
		end
		local ux, uy, uz = cross(bx, by, bz, rx, ry, rz, scale_z)
		self[1], self[2], self[3] = bx, rx, ux
		self[5], self[6], self[7] = by, ry, uy
		self[9], self[10],self[11]= bz, rz, uz
	end
	return self
end

function matrix:multiply(x, y, z, w)
	if (getmetatable(x) == "fast_mutable_matrix") then
		local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = m_unpack(self)
		local b11, b12, b13, b14, b21, b22, b23, b24, b31, b32, b33, b34, b41, b42, b43, b44 = m_unpack(x)
		-- first row
		self[1] = a11*b11 + a12*b21 + a13*b31 + a14*b41
		self[2] = a11*b12 + a12*b22 + a13*b32 + a14*b42
		self[3] = a11*b13 + a12*b23 + a13*b33 + a14*b43
		self[4] = a11*b14 + a12*b24 + a13*b34 + a14*b44
		-- second row
		self[5] = a21*b11 + a22*b21 + a23*b31 + a24*b41
		self[6] = a21*b12 + a22*b22 + a23*b32 + a24*b42
		self[7] = a21*b13 + a22*b23 + a23*b33 + a24*b43
		self[8] = a21*b14 + a22*b24 + a23*b34 + a24*b44
		-- third row
		self[9] = a31*b11 + a32*b21 + a33*b31 + a34*b41
		self[10]= a31*b12 + a32*b22 + a33*b32 + a34*b42
		self[11]= a31*b13 + a32*b23 + a33*b33 + a34*b43
		self[12]= a31*b14 + a32*b24 + a33*b34 + a34*b44
		-- fourth row
		self[13]= a41*b11 + a42*b21 + a43*b31 + a44*b41
		self[14]= a41*b12 + a42*b22 + a43*b32 + a44*b42
		self[15]= a41*b13 + a42*b23 + a43*b33 + a44*b43
		self[16]= a41*b14 + a42*b24 + a43*b34 + a44*b44
		return self
	else
		w = w or 1
		return
			x * self[1] + y * self[2] + z * self[3] + w * self[4],
			x * self[5] + y * self[6] + z * self[7] + w * self[8],
			x * self[9] + y * self[10]+ z * self[11]+ w * self[12],
			x * self[13]+ y * self[14]+ z * self[15]+ w * self[16]
	end
end

function matrix:invert()
	local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = m_unpack(self)
	-- normalize rotation
	a11, a21, a31 = normal(a11, a21, a31, 1)
	a12, a22, a32 = normal(a12, a22, a32, 1)
	a13, a23, a33 = normal(a13, a23, a33, 1)
	-- first row
	local i11 =  a22*a33*a44 - a22*a34*a43 - a32*a23*a44 + a32*a24*a43 + a42*a23*a34 - a42*a24*a33
	local i12 = -a12*a33*a44 + a12*a34*a43 + a32*a13*a44 - a32*a14*a43 - a42*a13*a34 + a42*a14*a33
	local i13 =  a12*a23*a44 - a12*a24*a43 - a22*a13*a44 + a22*a14*a43 + a42*a13*a24 - a42*a14*a23
	local i14 = -a12*a23*a34 + a12*a24*a33 + a22*a13*a34 - a22*a14*a33 - a32*a13*a24 + a32*a14*a23
	-- second row
	local i21 = -a21*a33*a44 + a21*a34*a43 + a31*a23*a44 - a31*a24*a43 - a41*a23*a34 + a41*a24*a33
	local i22 =  a11*a33*a44 - a11*a34*a43 - a31*a13*a44 + a31*a14*a43 + a41*a13*a34 - a41*a14*a33
	local i23 = -a11*a23*a44 + a11*a24*a43 + a21*a13*a44 - a21*a14*a43 - a41*a13*a24 + a41*a14*a23
	local i24 =  a11*a23*a34 - a11*a24*a33 - a21*a13*a34 + a21*a14*a33 + a31*a13*a24 - a31*a14*a23
	-- third row
	local i31 =  a21*a32*a44 - a21*a34*a42 - a31*a22*a44 + a31*a24*a42 + a41*a22*a34 - a41*a24*a32
	local i32 = -a11*a32*a44 + a11*a34*a42 + a31*a12*a44 - a31*a14*a42 - a41*a12*a34 + a41*a14*a32
	local i33 =  a11*a22*a44 - a11*a24*a42 - a21*a12*a44 + a21*a14*a42 + a41*a12*a24 - a41*a14*a22
	local i34 = -a11*a22*a34 + a11*a24*a32 + a21*a12*a34 - a21*a14*a32 - a31*a12*a24 + a31*a14*a22
	-- fourth row
	local i41 = -a21*a32*a43 + a21*a33*a42 + a31*a22*a43 - a31*a23*a42 - a41*a22*a33 + a41*a23*a32
	local i42 =  a11*a32*a43 - a11*a33*a42 - a31*a12*a43 + a31*a13*a42 + a41*a12*a33 - a41*a13*a32
	local i43 = -a11*a22*a43 + a11*a23*a42 + a21*a12*a43 - a21*a13*a42 - a41*a12*a23 + a41*a13*a22
	local i44 =  a11*a22*a33 - a11*a23*a32 - a21*a12*a33 + a21*a13*a32 + a31*a12*a23 - a31*a13*a22
	-- determinant
	if ((a11*i11 + a12*i21 + a13*i31 + a14*i41) ~= 0) then
		self[1], self[2], self[3], self[4] = i11, i12, i13, i14
		self[5], self[6], self[7], self[8] = i21, i22, i23, i24
		self[9], self[10],self[11],self[12]= i31, i32, i33, i34
		self[13],self[14],self[15],self[16]= i41, i42, i43, i44
	end
	return self
end

function matrix:determinant()
	local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = m_unpack(self)
	return
		  a11 * (a22*a33*a44 + a23*a34*a42 + a24*a32*a43
				-a24*a33*a42 - a22*a34*a43 - a23*a32*a44)
		- a12 * (a21*a33*a44 + a23*a34*a41 + a24*a31*a43
				-a24*a33*a41 - a21*a34*a43 - a23*a31*a44)
		+ a13 * (a21*a32*a44 + a22*a34*a41 + a24*a31*a42
				-a24*a32*a41 - a21*a34*a42 - a22*a31*a44)
		- a14 * (a21*a32*a43 + a22*a33*a41 + a23*a31*a42
				-a23*a32*a41 - a21*a33*a42 - a22*a31*a43)
end

function matrix:copyTo(other)
	for i = 1, 16 do
		other[i] = self[i]
	end
	return other
end

function matrix:copyFrom(other)
	for i = 1, 16 do
		self[i] = other[i]
	end
	return self
end

function matrix:duplicate()
	local new = matrix.new()
	for i = 1, 16 do
		new[i] = self[i]
	end
	return new
end

return matrix
