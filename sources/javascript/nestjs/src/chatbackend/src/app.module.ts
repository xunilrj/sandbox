import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService, AppService2, IAppService } from './app.service';
import { ioc } from './ioc';
import { log } from './interceptors';

@Module({
  imports: [],
  controllers: [AppController],
  providers: ioc({}, {log})
    .transient(AppService2)
    .transient(AppService, IAppService)
    .build() 
})
export class AppModule {}
