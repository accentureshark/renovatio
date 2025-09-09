package org.shark.renovatio.core.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import org.shark.renovatio.core.dto.UserDto;
import org.shark.renovatio.core.entity.UserEntity;

/**
 * Mapper for converting between {@link UserEntity} and {@link UserDto}.
 */
@Mapper
public interface UserMapper {

    UserMapper INSTANCE = Mappers.getMapper(UserMapper.class);

    UserDto toDto(UserEntity entity);

    UserEntity toEntity(UserDto dto);
}
