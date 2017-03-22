function Get-ConnectionString($Path, $Connection){
    $configMap = [System.Configuration.ExeConfigurationFileMap]::new();
    $configMap.ExeConfigFilename = $Path;

    $config = [System.Configuration.ConfigurationManager]::OpenMappedExeConfiguration($configMap, "None");
    $connection = $config.ConnectionStrings.ConnectionStrings[$Connection]

    Add-Type -AssemblyName System.Data.Entity
    
    $connectionFactory = [System.Data.EntityClient.EntityProviderFactory]::Instance
    $connectionStringBuilder = [System.Data.EntityClient.EntityConnectionStringBuilder]::new($connection.ConnectionString)
    
    $connectionFactory = [System.Data.Common.DbProviderFactories]::GetFactory($connectionStringBuilder.Provider)
    $finalConnectionStringBuilder = [System.Data.SqlClient.SqlConnectionStringBuilder]::new($connectionStringBuilder.ProviderConnectionString)

    New-Object PSCustomObject -Property @{
        ServerInstance = $finalConnectionStringBuilder.DataSource;
        Database = $finalConnectionStringBuilder.InitialCatalog;
        Username = $finalConnectionStringBuilder.UserID;
        Password = $finalConnectionStringBuilder.Password;        
    }
}

Export-ModuleMember -Function Get-ConnectionString