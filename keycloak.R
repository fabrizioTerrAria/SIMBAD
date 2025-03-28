library(httr)
library(jsonlite)

keycloackUrl <- fromJSON("config.json")$keycloak$url
realmName <- fromJSON("config.json")$keycloak$realmName
cliendId <- fromJSON("config.json")$keycloak$cliendId
clientSecret <- fromJSON("config.json")$keycloak$clientSecret
userMailAdmin <- fromJSON("config.json")$mail$address
smtpServer <- fromJSON("config.json")$mail$smtpServer
urlSimbad <- fromJSON("config.json")$site$url

getAccessToken <- function(keycloackUrl,realmName,cliendId,clientSecret) {
	
	headers = c(
		`Content-Type` = "application/x-www-form-urlencoded"
	)
	
	data = list(
		`grant_type` = "client_credentials",
		`client_id` = cliendId,
		`client_secret` = clientSecret
	)
	
	url <- paste0(keycloackUrl,"/realms/",realmName,"/protocol/openid-connect/token")
	
	res <- httr::POST(url = url, 
										httr::add_headers(.headers=headers), 
										body = data, 
										encode = "form")
	
	print(paste0(Sys.time()," - Access Token request. Status: ", res$status_code))
	
	if (res$status_code == 200) {
		
		return(content(res)$access_token)
		
	} else {
		
		return(NULL)
		
	}

}

createNewUser <- function(keycloackUrl,realmName,userData,userPsw,userGroup) {
	
	accessToken <- getAccessToken(keycloackUrl,realmName,cliendId,clientSecret)
	
	headers = c(
		`Content-Type` = "application/json",
		`Authorization` = paste0("Bearer ", accessToken)
	)
	
	username <- userData$username
	email <- userData$email
	firstName <-  userData$firstName
	lastName <- userData$lastName
	group <- userGroup

	data = paste0('{"enabled":true,"username":"',username,
								'","email":"',email,
								'","firstName":"',firstName,
								'","lastName":"',lastName,
								'","groups":["',group,
								'"],"credentials":[{"type":"password","value":"',userPsw,
								'","temporary":false}]}')
	
	url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/users")
	
	res <- httr::POST(url = url, 
										httr::add_headers(.headers=headers), 
										body = data
										)
	
	print(paste0(Sys.time()," - Create new user request. Status: ", res$status_code))
	
	if (res$status_code == 201) {
		
		return(201)
		
	} else {
		
		return(NULL)
		
	}
	
}

getListUsers <- function(keycloackUrl,realmName) {
	
	accessToken <- getAccessToken(keycloackUrl,realmName,cliendId,clientSecret)
	
	headers = c(
		`Authorization` = paste0("Bearer ", accessToken)
	)
	
	url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/users")
	
	res <- httr::GET(url = url, 
										httr::add_headers(.headers=headers)
	)
	
	print(paste0(Sys.time()," - Get users list. Status: ", res$status_code))
	
	if (res$status_code == 200) {
		
		return(fromJSON(httr::content(res, as = 'text', encoding = "UTF-8")))
		
	} else {
		
		return(NULL)
		
	}
	
}

getListUsersWithGroups <- function(keycloackUrl,realmName) {
	
	accessToken <- getAccessToken(keycloackUrl,realmName,cliendId,clientSecret)
	
	headers = c(
		`Authorization` = paste0("Bearer ", accessToken)
	)
	
	url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/groups")
	
	res <- httr::GET(url = url, 
									 httr::add_headers(.headers=headers)
	)
	
	print(paste0(Sys.time()," - Get group list. Status: ", res$status_code))
	
	groupListDF <- fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"))
	
	userList <- getListUsers(keycloackUrl,realmName)
	
	for (i in 1:length(groupListDF$id)) {
		
		url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/groups/",groupListDF$id[i],"/members")
		
		res <- httr::GET(url = url, 
										 httr::add_headers(.headers=headers)
		)
		
		userListTemp <- fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"))
		
		if (length(userListTemp)>0 & length(userList)>0) {
			
			userList$group[userList$id %in% userListTemp$id] <- groupListDF$name[i]
			# userList <- rbind(userList, userListTemp)
			
		} 
		
	}
	
	# gli utenti che si sono registrati dalla pagina di login non hanno il gruppo associato
	# li associo al gruppo di utenti semplici (USER)
	# per creare utenti admin bisogna passare dall'applicazione oppure da keycloak
	
	userList$group[is.na(userList$group)] <- "USER"
	
	if (res$status_code == 200) {
		
		return(userList)
		
	} else {
		
		return(NULL)
		
	}
	
}

deleteUser <- function(keycloackUrl,realmName,idUser) {
	
	accessToken <- getAccessToken(keycloackUrl,realmName,cliendId,clientSecret)
	
	headers = c(
		`Authorization` = paste0("Bearer ", accessToken)
	)
	
	url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/users/",idUser)
	
	res <- httr::DELETE(url = url, 
											httr::add_headers(.headers=headers)
	)
	
	print(paste0(Sys.time()," - Delete user. Status: ", res$status_code))
	
	
	if (res$status_code == 204) {
		
		return(204)
		
	} else {
		
		return(NULL)
		
	}
	
}

editUser <- function(keycloackUrl,realmName,idUser,userData,userPsw,userGroup) {
	
	accessToken <- getAccessToken(keycloackUrl,realmName,cliendId,clientSecret)
	
	headers = c(
		`Content-Type` = "application/json",
		`Authorization` = paste0("Bearer ", accessToken)
	)

	username <- userData$username
	email <- userData$email
	firstName <-  userData$firstName
	lastName <- userData$lastName
	group <- userGroup
	
	if (is.na(userPsw)) {
		
		data = paste0('{"enabled":true,"username":"',username,
									'","email":"',email,
									'","firstName":"',firstName,
									'","lastName":"',lastName,
									'","groups":["',group,
									'"]}')
		
	} else {
		
		data = paste0('{"enabled":true,"username":"',username,
									'","email":"',email,
									'","firstName":"',firstName,
									'","lastName":"',lastName,
									'","groups":["',group,
									'"],"credentials":[{"type":"password","value":"',userPsw,
									'","temporary":false}]}')
		
	}
	
	url <- paste0(keycloackUrl,"/admin/realms/",realmName,"/users/",idUser)
	
	res <- httr::PUT(url = url, 
									 httr::add_headers(.headers=headers), 
									 body = data
	)
	
	print(paste0(Sys.time()," - Edit user. Status: ", res$status_code))
	
	
	if (res$status_code == 204) {
		
		return(204)
		
	} else {
		
		return(NULL)
		
	}
	
}

	
	
