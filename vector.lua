local vector = {}

local newVector

local function __newindex()
	error("Vector is immutable")
end

local function __tostring(self)
	return string.format("%f, %f, %f", self.x, self.y, self.z)
end

local function __len(self)
	return self.magnitude
end

local function __unm(self)
	return newVector(
		-self.x,
		-self.y,
		-self.z,
		false
	)
end

local function __add(self, other)
	if (getmetatable(self) == "vector") then
		if (getmetatable(other) == "vector") then
			return newVector(
				self.x + other.x,
				self.y + other.y,
				self.z + other.z,
				false
			)
		elseif (type(other) == "number") then
			return newVector(
				self.x + other,
				self.y + other,
				self.z + other,
				false
			)
		else
			error("Attempt to add Vector and " .. type(other))
		end
	elseif (type(self) == "number") then
		return newVector(
			self + other.x,
			self + other.y,
			self + other.z,
			false
		)
	else
		error("Attempt to add " .. type(other) .. " and Vector")
	end
end

local function __sub(self, other)
	if (getmetatable(self) == "vector") then
		if (getmetatable(other) == "vector") then
			return newVector(
				self.x - other.x,
				self.y - other.y,
				self.z - other.z,
				false
			)
		elseif (type(other) == "number") then
			return newVector(
				self.x - other,
				self.y - other,
				self.z - other,
				false
			)
		else
			error("Attempt to subtract Vector and " .. type(other))
		end
	elseif (type(self) == "number") then
		return newVector(
			self - other.x,
			self - other.y,
			self - other.z,
			false
		)
	else
		error("Attempt to subtract " .. type(other) .. " and Vector")
	end
end

local function __mul(self, other)
	if (getmetatable(self) == "vector") then
		if (getmetatable(other) == "vector") then
			return newVector(
				self.x * other.x,
				self.y * other.y,
				self.z * other.z,
				false
			)
		elseif (type(other) == "number") then
			return newVector(
				self.x * other,
				self.y * other,
				self.z * other,
				false
			)
		else
			error("Attempt to multiply Vector and " .. type(other))
		end
	elseif (type(self) == "number") then
		return newVector(
			self * other.x,
			self * other.y,
			self * other.z,
			false
		)
	else
		error("Attempt to multiply " .. type(self) .. " and Vector")
	end
end

local function __div(self, other)
	if (getmetatable(self) == "vector") then
		if (getmetatable(other) == "vector") then
			return newVector(
				self.x / other.x,
				self.y / other.y,
				self.z / other.z,
				false
			)
		elseif (type(other) == "number") then
			return newVector(
				self.x / other,
				self.y / other,
				self.z / other,
				false
			)
		else
			error("Attempt to divide Vector and " .. type(other))
		end
	elseif (type(self) == "number") then
		return newVector(
			self / other.x,
			self / other.y,
			self / other.z,
			false
		)
		else
		error("Attempt to divide " .. type(self) .. " and Vector")
	end
end


newVector = function(x, y, z, isUnit)
	local self = setmetatable({
		x = x,
		y = y,
		z = z,
		magnitude = math.sqrt(x * x + y * y + z * z)
	}, {
		__index = vector
	})
	local proxy = setmetatable({}, {
		__index = self,
		__newindex = __newindex,
		__metatable = "vector",
		__tostring = __tostring,
		__len = __len,
		__unm = __unm,
		__add = __add,
		__sub = __sub,
		__mul = __mul,
		__div = __div
	})
	self.unit = (isUnit or self.magnitude == 0) and proxy or newVector(x / self.magnitude, y / self.magnitude, z / self.magnitude, true)
	self.addr = tostring(self)
	return proxy
end

function vector:dot(other)
	return self.x * other.x + self.y * other.y + self.z * other.z
end

function vector:cross(other)
	assert(getmetatable(self) == "vector", "Bad argument #0 to Vector.cross, expected Vector, got " .. type(self))
	assert(getmetatable(other) == "vector", "Bad argument #1 to Vector.cross, expected Vector, got " .. type(other))
	return newVector(
		self.y * other.z - self.z * other.y,
		self.z * other.x - self.x * other.z,
		self.x * other.y - self.y * other.x,
		false
	)
end

function vector:lerp(other, t)
	assert(getmetatable(self) == "vector", "Bad argument #0 to Vector.lerp, expected Vector, got " .. type(self))
	assert(getmetatable(other) == "vector", "Bad argument #1 to Vector.lerp, expected Vector, got " .. type(other))
	assert(type(t) == "number", "Bad argument #2 to vector.lerp, expected number, got " .. type(t))
	return newVector(
		self.x + (other.x - self.x) * t,
		self.y + (other.y - self.y) * t,
		self.z + (other.z - self.z) * t,
		false
	)
end

function vector.new(x, y, z)
	if (x == nil) then
		return newVector(0, 0, 0, false)
	else
		assert(type(x) == "number", "Bad argument #1 to vector.new, expected number, got " .. type(x))
		assert(type(y) == "number", "Bad argument #1 to vector.new, expected number, got " .. type(y))
		assert(type(z) == "number", "Bad argument #1 to vector.new, expected number, got " .. type(z))
		return newVector(x, y, z, false)
	end
end

return vector
