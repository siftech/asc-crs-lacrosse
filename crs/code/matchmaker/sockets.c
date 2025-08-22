/******************************************************************************
* 				
* File:         sockets.c
* Author:       Michael Hucka  (much changed by D. Musliner)
* 10/23 DJM : removed dependence on remote custom status and debug garbage
*  	DJM : modified so return value is socket id, when new socket made
* 10/31 DJM : cleaning up modifications, removing traces of debug garbage
******************************************************************************/

#define _GNU_SOURCE

#include <sys/types.h>
#include <errno.h> 
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <string.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#ifndef __CYGWIN__
#ifndef CYGWIN
#ifndef LINUX
#include <sys/filio.h>
#endif
#endif
#endif

#include <stdio.h>

#define fatal_error(X) {perror(X); fflush(stderr); return(-1);}

/************************************************************
  - Create and initialize a new socket and return it.
  - Useful for creating server-side sockets.
************************************************************/
int
socket_init(int port, int max_req)
{
  int    sock;
  int    options = 1;      
  struct sockaddr_in local;
  
  if (port < 0) fatal_error("socket_init: bad port");
  if (max_req > 1024) {
    /* listen() backlog on a modern linux machine defaults to 128. 
     * However, we've tuned our servers to be 1024. 
    */
    fatal_error("socket_init: queue length too high");
  }
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fatal_error("socket_init: connection refused");
  }

  /* Set the SO_KEEPALIVE socket option which causes the system to keep
   * otherwise idle connections alive by periodically transmitting
   * messages.  If the connected peer fails to respond to these messages,
   * the connection is aborted.  Usually the system will transmit a null
   * message if a minute passes without any activity on the socket.  The
   * system then waits four minutes for a response.  If four minutes pass
   * without a response, the socket is marked closed.  On true UNIX
   * systems, optval serves as a toggle to enable / disable the option.
   * Here we enable the SO_KEEPALIVE option.
   */
  if (setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE, (void *)&options, sizeof(options)) < 0) {
    socket_close(sock);
    fatal_error("socket_init: setsockopt SO_KEEPALIVE error");
  }

	/* DJM 9/13/00 This should be the miraculous avoid-TIME_WAIT fix ! */
  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (void *)&options, sizeof(options)) < 0) {
    socket_close(sock);
    fatal_error("socket_init: setsockopt SO_REUSEADDR error");
  }

  if (setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (void *) &options, sizeof(options)) <0) {
    socket_close(sock);
    fatal_error("socket_init: setsockopt SO_REUSEADDR error");
  }

  /* Name socket using wildcards */
  bzero((char *)&local, sizeof(local)); /* Clear socket address structure */
  local.sin_family = AF_INET;	   /* Internet domain socket */
  local.sin_addr.s_addr = htonl((long)INADDR_ANY);
  local.sin_port = htons(port);
#ifdef __GNUC__
  if (bind(sock, (struct sockaddr *)&local, sizeof(local)) < 0) {
#else
  if (bind(sock, &local, sizeof(local)) < 0) {
#endif /* GNUC */
    if ((errno == EADDRINUSE) || (errno == EINVAL)) {
      fatal_error("socket_init: socket address in use");
    }
    else {
      fatal_error("socket_init: socket bind failed");
    }
  }
  else if (listen(sock, max_req) < 0) {
    fatal_error("socket_init: listen failure");
  } 
  else return(sock);
} /* end of socket_init */



/************************************************************
 Accepts the first connection waiting on the queue of connections pending
  for socket SOCK.  Once a connection is received, a new socket NEW_SOCK is
  created, with the same properties as the socket SOCK.  The accepted 
  socket, NEW_SOCK , cannot be used to accept more connections.  The 
  original socket SOCK remains open. 
 
 If no pending connections are present on the queue, and the socket is not 
 marked as non-blocking, socket_accept blocks the caller until a connection
 is present.  If the socket is marked non-blocking and no pending
 connections are present on the queue, socket_accept returns the status
 code "socket_would_block."  

  DJM : removed new_sock from args, made it return that on success.
************************************************************/

int socket_accept(sock)
  int sock;
{
  int new_sock;

  errno = 0;
  if ((new_sock = accept(sock, 0, 0)) < 0) { 
    if ((errno == EBADF) || (errno == ENOTSOCK) || (errno == EFAULT))
      fatal_error("socket_accept: illegal arg")
	else
	  fatal_error("socket_accept: socket would block")
	}
  return(new_sock);
} /* end of socket_accept */

/************************************************************
 - ripped off from Joe Dionese's sock_rtn.c
 - DJM : removed sock from args, made it return sock on success.
************************************************************/

int socket_connect(hostname, port)
  char hostname[];
int  port;                           
{            
  int  sock;   
  struct sockaddr_in remote;	     /* Remote machine socket addr structure.*/
  struct hostent *remote_addr;
  struct hostent *gethostbyname();
  struct linger ling;

  /* Try to get address of the remote machine from the /etc/hosts file. */
  remote_addr = gethostbyname(hostname);  
  if (remote_addr == 0) {         
    fatal_error("socket_connect: unknown host");
  }

  /* Attempt to open the socket.  Note the address format is internet.    */
  /* Also the connection type is a socket stream, which provides          */
  /* potentially reliable full-duplex communication between two machines. */
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fatal_error("socket_connect: socket call failed");
  }

  /* Clear the remote machine socket address structure. */
  bzero((char *)&remote, sizeof(remote)); 

  /* We will live in the Internet domain. */
  remote.sin_family = AF_INET;
               
  /* Convert the remote machine address into the internet address format. */
  bcopy(remote_addr->h_addr,(char *)&remote.sin_addr, remote_addr->h_length);
                              
  /* This is the port number we assume the remote machine is monitoring.  */
  remote.sin_port = htons(port); 

  ling.l_onoff=1;
  ling.l_linger=30;
  if (setsockopt(sock, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling)) < 0) {
    socket_close(sock);
    fatal_error("socket_init: setsockopt SO_LINGER error");
  }

  /* We now have a nameless socket, bind it to a local address and port,  */
  /* and a remote address and port.  Connect does local bind for us free.  */
  if (connect(sock, (struct sockaddr *)&remote, sizeof(remote)) < 0) {
    fatal_error("socket_connect: connect failed");
  }

  return(sock);		/* DJM : successful return = sock */
} /* end of socket_connect */


/************************************************************/
int socket_read(sock, buf, buf_len, read_len)
  int sock;
char buf[];
int buf_len;
int *read_len;
{
  if ((buf_len < 0) || (sock < 0))	/* Sanity check. */
    fatal_error("socket_read: sock arg or buf len error" );

  errno = 0;				/* Must clear this explicitly. */
  /*
   * The read will block if the socket hasnt been marked as non-blocking and 
   * the number of bytes attempted to be read is > 0.  The pathological
   * case of buf_len = 0 will return successfully with *read_len = 0.
   */
  *read_len = read(sock, buf, buf_len);
  if (*read_len == 0) 		/* Connection closed. */
    { fprintf(stderr,"socket_read: socket_no_connection\n"); fflush(stderr); return(-1); }  
	// didnt use fatal_error above b/c perror return code printout makes it spuriously print "Success"
  else if (*read_len == -1) 
    if (errno == EWOULDBLOCK) 
      /* This is okay -- means no data to read on non-blocking socket. */
      {
	*read_len = 0;
	return(0);
      }
    else 
      {
	*read_len = 0;
	fatal_error("socket_read: read");
      }
  return(0);
} /* end of socket_read */
	  
/************************************************************/
int socket_write(sock, buf, buf_len, write_len)
  int sock;
char buf[];
int buf_len;
int *write_len;
{
  /*printf("writing to sock %d.\n", sock);*/

  if ((buf_len < 0) || (sock < 0))	/* Sanity check. */
    fatal_error("socket_write: socket_illegal_argument");

  errno = 0;				/* Must clear this explicitly. */
  *write_len = write(sock,buf,buf_len);
  if (*write_len > 0) 		/* Partial or complete write. */
    return(0);
  else if (*write_len == 0)		/* Connection closed. */
    {  
      perror("socket_write problem");
      fatal_error("socket_write: socket_no_connection");
    }
  else if ((*write_len == -1) && (errno == EWOULDBLOCK)) {
    /* This okay :means have nothing to write on non-blocking socket. */
    *write_len = 0;
    return(0);
  } else {
    *write_len = 0;
    fatal_error("socket_write: socket_internal_error");
  }
} /* end of socket_write */

/************************************************************/
int socket_nonblocking(sock)
  int sock;
{
  int one = 1;
  int rval;
  rval = ioctl(sock,FIONBIO,&one);
  if (rval<0)
    { fatal_error("socket_nonblocking failed"); }
  return(rval);
}

int socket_blocking(int sock) {
  int zero = 0;
  int rval;
  rval = ioctl(sock, FIONBIO, &zero);
  if (rval<0)
    { fatal_error("socket_blocking failed"); }
  return(rval);
}


/************************************************************/
int socket_close(sock)
  int sock;
{

  if (sock < 0)			/* Sanity check. */
    fatal_error("socket_close: illegal argument");

  if (shutdown(sock, 2) < 0) 	/* Disallow further send & rec. */
         { perror("socket_close: shutdown failed"); }
  if (close(sock) < 0) 
    { fatal_error("socket_close: close failed"); }
  return(1);
} /* end of socket_close */


