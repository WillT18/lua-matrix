local vector = require("vector")
local matrix = {}

local function normalize(x, y, z)
	local l = math.sqrt(x*x + y*y + z*z)
	return x/l, y/l, z/l, l
end

local function normal_cross(x, y, z, a, b, c)
	local d, e, f = y*c - z*b, z*a - x*c, x*b - y*a
	local l = math.sqrt(d*d + e*e + f*f)
	return d/l, e/l, f/l, l
end

local vector_calculate = vector.calculate

---@diagnostic disable-next-line: deprecated
local current_unpack = unpack or table.unpack

local meta = {
	__metatable = "mutable_matrix",
	__index = matrix,
	__newindex = function() end,
	__tostring = function(a)
		return string.format("%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f", current_unpack(a))
	end,
	__add = function(a, b)
		local t1, t2 = getmetatable(a), getmetatable(b)
		if (t1 == "mutable_matrix") then
			if (t2 == "mutable_matrix") then
				return matrix.calculate(matrix.setPosition(matrix.duplicate(a), a.position[1] + b.position[1], a.position[2] + b.position[2], a.position[3] + b.position[3]))
			elseif (t2 == "immutable_vector") then
				return matrix.calculate(matrix.setPosition(matrix.duplicate(a), a.position[1] + b[1], a.position[2] + b[2], a.position[3] + b[3]))
			end
		elseif (t1 == "immutable_vector") then
			return matrix.calculate(matrix.setPosition(matrix.duplicate(b), a[1] + b.position[1], a[2] + b.position[2], a[3] + b.position[3]))
		end
	end,
	__sub = function(a, b)
		local t1, t2 = getmetatable(a), getmetatable(b)
		if (t1 == "mutable_matrix") then
			if (t2 == "mutable_matrix") then
				return matrix.calculate(matrix.setPosition(matrix.duplicate(a), a.position[1] - b.position[1], a.position[2] - b.position[2], a.position[3] - b.position[3]))
			elseif (t2 == "immutable_vector") then
				return matrix.calculate(matrix.setPosition(matrix.duplicate(a), a.position[1] - b[1], a.position[2] - b[2], a.position[3] - b[3]))
			end
		elseif (t1 == "immutable_vector") then
			return matrix.calculate(matrix.setPosition(matrix.duplicate(b), a[1] - b.position[1], a[2] - b.position[2], a[3] - b.position[3]))
		end
	end,
	__unm = function(a)
		return matrix.invert(matrix.duplicate(a))
	end,
	__mul = function(a, b)
		local t1, t2 = getmetatable(a), getmetatable(b)
		if (t1 == "mutable_matrix") then
			if (t2 == "mutable_matrix") then
				return matrix.multiply(matrix.duplicate(a), b)
			elseif (t2 == "immutable_vector") then
				return vector.new(matrix.vectorMultiply(a, b[1], b[2], b[3], 1))
			end
		elseif (t1 == "immutable_vector") then
			return vector.new(matrix.vectorMultiply(b, a[1], a[2], a[3], 1))
		end
	end,
	__div = function(a, b)
		local t1, t2 = getmetatable(a), getmetatable(b)
		if (t1 == "mutable_matrix") then
			if (t2 == "mutable_matrix") then
				return matrix.multiply(matrix.invert(matrix.duplicate(a)), b)
			elseif (t2 == "immutable_vector") then
				return vector.new(matrix.vectorMultiply(matrix.invert(matrix.duplicate(a)), b[1], b[2], b[3], 1))
			end
		end
	end,
	__len = function(a)
		return a.position.magnitude
	end
}

function matrix:calculate()
	self[1], self[2], self[3], self[4] = -self.forward[1]*self.scale[1], self.right[1]*self.scale[2], self.up[1]*self.scale[3], self.position[1]
	self[5], self[6], self[7], self[8] = -self.forward[2]*self.scale[1], self.right[2]*self.scale[2], self.up[2]*self.scale[3], self.position[2]
	self[9], self[10],self[11],self[12]= -self.forward[3]*self.scale[1], self.right[3]*self.scale[2], self.up[3]*self.scale[3], self.position[3]
	self[13],self[14],self[15],self[16]= 0, 0, 0, 1
	return self
end

function matrix.new()
	return setmetatable({
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
		position = vector.new(0, 0, 0),
		scale = vector.new(1, 1, 1),
		forward = vector.new(-1, 0, 0),
		right = vector.new(0, 1, 0),
		up = vector.new(0, 0, 1)
	}, meta)
end

function matrix:reset()
	vector_calculate(self.position, 0, 0, 0)
	vector_calculate(self.scale, 1, 1, 1)
	vector_calculate(self.forward, -1, 0, 0)
	vector_calculate(self.right, 0, 1, 0)
	vector_calculate(self.up, 0, 0, 1)
	return self
end

function matrix:setPosition(x, y, z)
	vector_calculate(self.position, x, y, z)
	return self
end

function matrix:setScale(x, y, z)
	vector_calculate(self.scale, x, y, z)
	return self
end

function matrix:setRotation(roll, pitch, yaw)
	local ca, cb, cc = math.cos(yaw), math.cos(pitch), math.cos(roll)
	local sa, sb, sc = math.sin(yaw), math.sin(pitch), math.sin(roll)
	vector_calculate(self.forward, -ca*cb, -sa*cb, sb)
	vector_calculate(self.right, ca*sb*sc - sa*cc, sa*sb*sc + ca*cc, cb*sc)
	vector_calculate(self.up, ca*sb*cc + sa*sc, sa*sb*cc - ca*sc, cb*cc)
	return self
end

function matrix:setTarget(x, y, z, upx, upy, upz)
	local fx, fy, fz, fl = normalize(x - self.position[1], y - self.position[2], z - self.position[3])
	if (fl > 0) then
		-- default up direction is +Z
		local sx, sy, sz, sl = 0, 0, 1, 1
		if (upx ~= nil) then
			-- if up vector is specified use that
			sx, sy, sz, sl = normalize(upx, upy, upz)
			if (sl == 0) then
				-- if up is undefined revert back to default
				sx, sy, sz = 0, 0, 1
			end
		end
		local rx, ry, rz, rl = normal_cross(fx, fy, fz, sx, sy, sz)
		if (rl == 0) then
			-- look vector is parallel with up vector
			if (sx == 0 and sy == 0) then
				-- up vector is +/-Z, default to +X
				rx, ry, rz, _ = normal_cross(fx, fy, fz, 1, 0, 0)
			else
				-- default up vector to +Z
				rx, ry, rz, _ = normal_cross(fx, fy, fz, 0, 0, 1)
			end
		end
		vector_calculate(self.forward, fx, fy, fz)
		vector_calculate(self.right, rx, ry, rz)
		vector_calculate(self.up, normal_cross(rx, ry, rz, fx, fy, fz))
	end
	return self
end

function matrix:multiply(other)
	local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = current_unpack(self)
	local b11, b12, b13, b14, b21, b22, b23, b24, b31, b32, b33, b34, b41, b42, b43, b44 = current_unpack(other)
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

	vector_calculate(self.position, self[4], self[8], self[12])
	local bx, by, bz, sx = normalize(self[1], self[5], self[9])
	local rx, ry, rz, sy = normalize(self[2], self[6], self[10])
	local ux, uy, uz, sz = normalize(self[3], self[7], self[11])
	vector_calculate(self.forward, -bx, -by, -bz)
	vector_calculate(self.right, rx, ry, rz)
	vector_calculate(self.up, ux, uy, uz)
	vector_calculate(self.scale, sx, sy, sz)
	return self
end

function matrix:vectorMultiply(x, y, z, w)
	w = w or 1
	return
		x * self[1] + y * self[2] + z * self[3] + w * self[4],
		x * self[5] + y * self[6] + z * self[7] + w * self[8],
		x * self[9] + y * self[10]+ z * self[11]+ w * self[12],
		x * self[13]+ y * self[14]+ z * self[15]+ w * self[16]
end

function matrix:invert()
	local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = current_unpack(self)

	local i11 =  a22*a33*a44 - a22*a34*a43 - a32*a23*a44 + a32*a24*a43 + a42*a23*a34 - a42*a24*a33
	local i12 = -a12*a33*a44 + a12*a34*a43 + a32*a13*a44 - a32*a14*a43 - a42*a13*a34 + a42*a14*a33
	local i13 =  a12*a23*a44 - a12*a24*a43 - a22*a13*a44 + a22*a14*a43 + a42*a13*a24 - a42*a14*a23
	local i14 = -a12*a23*a34 + a12*a24*a33 + a22*a13*a34 - a22*a14*a33 - a32*a13*a24 + a32*a14*a23

	local i21 = -a21*a33*a44 + a21*a34*a43 + a31*a23*a44 - a31*a24*a43 - a41*a23*a34 + a41*a24*a33
	local i22 =  a11*a33*a44 - a11*a34*a43 - a31*a13*a44 + a31*a14*a43 + a41*a13*a34 - a41*a14*a33
	local i23 = -a11*a23*a44 + a11*a24*a43 + a21*a13*a44 - a21*a14*a43 - a41*a13*a24 + a41*a14*a23
	local i24 =  a11*a23*a34 - a11*a24*a33 - a21*a13*a34 + a21*a14*a33 + a31*a13*a24 - a31*a14*a23

	local i31 =  a21*a32*a44 - a21*a34*a42 - a31*a22*a44 + a31*a24*a42 + a41*a22*a34 - a41*a24*a32
	local i32 = -a11*a32*a44 + a11*a34*a42 + a31*a12*a44 - a31*a14*a42 - a41*a12*a34 + a41*a14*a32
	local i33 =  a11*a22*a44 - a11*a24*a42 - a21*a12*a44 + a21*a14*a42 + a41*a12*a24 - a41*a14*a22
	local i34 = -a11*a22*a34 + a11*a24*a32 + a21*a12*a34 - a21*a14*a32 - a31*a12*a24 + a31*a14*a22

	local i41 = -a21*a32*a43 + a21*a33*a42 + a31*a22*a43 - a31*a23*a42 - a41*a22*a33 + a41*a23*a32
	local i42 =  a11*a32*a43 - a11*a33*a42 - a31*a12*a43 + a31*a13*a42 + a41*a12*a33 - a41*a13*a32
	local i43 = -a11*a22*a43 + a11*a23*a42 + a21*a12*a43 - a21*a13*a42 - a41*a12*a23 + a41*a13*a22
	local i44 =  a11*a22*a33 - a11*a23*a32 - a21*a12*a33 + a21*a13*a32 + a31*a12*a23 - a31*a13*a22

	local det = a11*i11 + a12*i21 + a13*i31 + a14*i41
	--assert(det ~= 0, "Matrix has no inverse.")
	if (det ~= 0) then
		self[1], self[2], self[3], self[4] = i11, i12, i13, i14
		self[5], self[6], self[7], self[8] = i21, i22, i23, i24
		self[9], self[10],self[11],self[12]= i31, i32, i33, i34
		self[13],self[14],self[15],self[16]= i41, i42, i43, i44

		vector_calculate(self.position, self[4], self[8], self[12])
		local bx, by, bz, bl = normalize(self[1], self[5], self[9])
		local rx, ry, rz, rl = normalize(self[2], self[6], self[10])
		local ux, uy, uz, ul = normalize(self[3], self[7], self[11])
		vector_calculate(self.forward, -bx, -by, -bz)
		vector_calculate(self.right, rx, ry, rz)
		vector_calculate(self.up, ux, uy, uz)
		vector_calculate(self.scale, bl, rl, ul)
	end
	return self
end

function matrix:determinant()
	local a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44 = current_unpack(self)
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
	for k, v in pairs(self) do
		if (type(k) == "number") then
			other[k] = v
		else
			vector_calculate(other[k], v[1], v[2], v[3])
		end
	end
	return other
end

function matrix:copyFrom(other)
	return matrix.copyTo(other, self)
end

function matrix:duplicate()
	return matrix.copyTo(self, matrix.new())
end

return matrix
