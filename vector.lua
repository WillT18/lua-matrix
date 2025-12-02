local vector = {}

local function sanitize(t)
	if (type(t) == "table") then
		return (tonumber(t[1]) or 0), (tonumber(t[2]) or 0), (tonumber(t[3]) or 0)
	elseif (type(t) == "number") then
		return t, t, t
	else
		return 0, 0, 0
	end
end

local meta = {
	__metatable = "immutable_vector",
	__index = vector,
	__newindex = function() end,
	__tostring = function(a)
		return string.format("(%f, %f, %f)", a[1], a[2], a[3])
	end,
	__add = function(a, b)
		local ax, ay, az = sanitize(a)
		local bx, by, bz = sanitize(b)
		return vector.new(ax + bx, ay + by, az + bz)
	end,
	__sub = function(a, b)
		local ax, ay, az = sanitize(a)
		local bx, by, bz = sanitize(b)
		return vector.new(ax - bx, ay - by, az - bz)
	end,
	__mul = function(a, b)
		local ax, ay, az = sanitize(a)
		local bx, by, bz = sanitize(b)
		return vector.new(ax * bx, ay * by, az * bz)
	end,
	__div = function(a, b)
		local ax, ay, az = sanitize(a)
		local bx, by, bz = sanitize(b)
		return vector.new(ax / bx, ay / by, az / bz)
	end,
	__unm = function(a)
		local ax, ay, az = sanitize(a)
		return vector.new(-ax, -ay, -az)
	end,
	__len = function(a)
		return a.magnitude
	end
}

function vector.new(x, y, z)
	if (type(x) == "table") then
		x, y, z = (tonumber(x[1]) or 0), (tonumber(x[2]) or 0), (tonumber(x[3]) or 0)
	end
	local unit = {
		0, 0, 0,
		magnitude = 0,
	}
	unit.unit = unit
	unit.addr = tostring(unit)
	local self = {
		x, y, z,
		magnitude = math.sqrt(x*x + y*y + z*z),
		unit = setmetatable(unit, meta)
	}
	self.addr = tostring(self)
	vector.calculate(self)
	return setmetatable(self, meta)
end

function vector:calculate(x, y, z)
	self[1] = tonumber(x) or self[1]
	self[2] = tonumber(y) or self[2]
	self[3] = tonumber(z) or self[3]
	self.magnitude = math.sqrt(self[1] * self[1] + self[2] * self[2] + self[3] * self[3])
	if (self.magnitude > 0) then
		self.unit[1] = self[1] / self.magnitude
		self.unit[2] = self[2] / self.magnitude
		self.unit[3] = self[3] / self.magnitude
		self.unit.magnitude = 1
	else
		self.unit[1] = 0
		self.unit[2] = 0
		self.unit[3] = 0
		self.unit.magnitude = 0
	end
end

function vector:dot(other)
	return self[1] * other[1] + self[2] * other[2] + self[3] * other[3]
end

function vector:cross(other)
	return vector.new(
		self[2] * other[3] - self[3] * other[2],
		self[3] * other[1] - self[1] * other[3],
		self[1] * other[2] - self[2] * other[1]
	)
end

return vector
