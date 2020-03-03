import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';


import { Injectable, NestInterceptor, ExecutionContext, CallHandler } from '@nestjs/common';
import { Observable } from 'rxjs';
import { tap } from 'rxjs/operators';



async function bootstrap() {
  const app = await NestFactory.create(AppModule);  
  await app.listen(3005);
}
bootstrap();
