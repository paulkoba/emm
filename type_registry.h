//
// Created by fail on 8/1/22.
//

#ifndef EMMC_TYPE_REGISTRY_H
#define EMMC_TYPE_REGISTRY_H

#include "type.h"

/*
 * This class is used to register types.
 * Types cannot be registered twice under the same name.
 * Types can not be removed from the registry.
 */
class TypeRegistry {
	std::unordered_map<std::string, Type*> types;

	void registerBuiltinTypes(llvm::IRBuilder<>& builder) {
		registerType(new Type(builder.getInt8Ty(), "i8", true, true));
		registerType(new Type(builder.getInt16Ty(), "i16", true, true));
		registerType(new Type(builder.getInt32Ty(), "i32", true, true));
		registerType(new Type(builder.getInt64Ty(), "i64", true, true));
		registerType(new Type(builder.getFloatTy(), "f32", true, true));
		registerType(new Type(builder.getDoubleTy(), "f64", true, true));
		registerType(new Type(builder.getInt8Ty(), "u8", true, false));
		registerType(new Type(builder.getInt16Ty(), "u16", true, false));
		registerType(new Type(builder.getInt32Ty(), "u32", true, false));
		registerType(new Type(builder.getInt64Ty(), "u64", true, false));
		registerType(new Type(builder.getInt1Ty(), "bool", true, false));
	}

   public:
	explicit TypeRegistry(llvm::IRBuilder<>& builder) { registerBuiltinTypes(builder); }

	void registerType(Type* type) { types[type->getName()] = type; }

	[[nodiscard]] Type* getType(const std::string& name) {
		auto it = types.find(name);
		if (it == types.end()) {
			return nullptr;
		}
		return it->second;
	}

	[[nodiscard]] bool isTypeRegistered(const std::string& name) { return types.find(name) != types.end(); }

	void registerType(const std::string& name, Type* type) { types[name] = type; }

	~TypeRegistry() {
		for (auto& pair : types) {
			delete pair.second;
		}
	}
};

std::unique_ptr<TypeRegistry> typeRegistry = nullptr;

// This will need to be part of ModuleAST, but I left it as a static function for now
TypeRegistry* getTypeRegistry() { return typeRegistry.get(); }

#endif	// EMMC_TYPE_REGISTRY_H
