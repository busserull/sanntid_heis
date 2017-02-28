#include "elev_macOs.h"

#include <assert.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <pthread.h>

#include "con_load.h"

#define MOTOR_SPEED 2800
static int sockfd;
static pthread_mutex_t sockmtx;

void elev_init(elev_type e) {
    assert(e==ET_Simulation && "There are no elevators here, use simulator!");
    char ip[16] = {0};
    char port[8] = {0};
    con_load("../elevator_driver/simulator/simulator.con",
        con_val("com_ip",   ip,   "%s")
        con_val("com_port", port, "%s")
    )
    
    pthread_mutex_init(&sockmtx, NULL);

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    assert(sockfd != -1 && "Unable to set up socket");

    struct addrinfo hints = {
        .ai_family      = AF_UNSPEC, 
        .ai_socktype    = SOCK_STREAM, 
        .ai_protocol    = IPPROTO_TCP,
    };
    struct addrinfo* res;
    getaddrinfo(ip, port, &hints, &res);

    int fail = connect(sockfd, res->ai_addr, res->ai_addrlen);
    assert(fail == 0 && "Unable to connect to simulator server");

    freeaddrinfo(res);

    send(sockfd, (char[4]) {0}, 4, 0);
}




void elev_set_motor_direction(elev_motor_direction_t dirn) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {1, dirn}, 4, 0);
        pthread_mutex_unlock(&sockmtx);
}


void elev_set_button_lamp(elev_button_type_t button, int floor, int value) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {2, button, floor, value}, 4, 0);
        pthread_mutex_unlock(&sockmtx);
}


void elev_set_floor_indicator(int floor) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {3, floor}, 4, 0);
        pthread_mutex_unlock(&sockmtx);
}


void elev_set_door_open_lamp(int value) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {4, value}, 4, 0);
        pthread_mutex_unlock(&sockmtx);
}


void elev_set_stop_lamp(int value) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {5, value}, 4, 0);
        pthread_mutex_unlock(&sockmtx);
}




int elev_get_button_signal(elev_button_type_t button, int floor) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {6, button, floor}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        pthread_mutex_unlock(&sockmtx);
        return buf[1];
}


int elev_get_floor_sensor_signal(void) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {7}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        pthread_mutex_unlock(&sockmtx);
        return buf[1] ? buf[2] : -1;
}


int elev_get_stop_signal(void) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {8}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        pthread_mutex_unlock(&sockmtx);
        return buf[1];
}


int elev_get_obstruction_signal(void) {
        pthread_mutex_lock(&sockmtx);
        send(sockfd, (char[4]) {9}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        pthread_mutex_unlock(&sockmtx);
        return buf[1];
}
