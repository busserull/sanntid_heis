#include "erl_nif.h"
#include "elev.h"


ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

int atomToButtonType(ErlNifEnv* env, const ERL_NIF_TERM argv, elev_button_type_t* type)
{
    if (enif_is_identical(argv, mk_atom(env,"up")))
    {
        *type = BUTTON_CALL_UP;
    }
    else if (enif_is_identical(argv, mk_atom(env,"down")))
    {
        *type = BUTTON_CALL_DOWN;
    }

    else if (enif_is_identical(argv, mk_atom(env,"internal")))
    {
        *type = BUTTON_COMMAND;
    }
    else 
    {
        return 0;
    }
    return 1;
}

int atomToBool(ErlNifEnv* env, const ERL_NIF_TERM argv, int* val)
{
    if (enif_is_identical(argv, mk_atom(env,"on")))
    {
        *val = 1;
    }
    else if (enif_is_identical(argv, mk_atom(env,"off")))
    {
        *val = 0;
    }
    else 
    {
        return 0;
    }
    return 1;
}



static ERL_NIF_TERM
initElevator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_is_identical(argv[0], mk_atom(env,"simulator")))
    {
        elev_init(ET_Simulation);
        return mk_atom(env, "ok");
    }
    else if (enif_is_identical(argv[0], mk_atom(env,"real")))
    {
        elev_init(ET_Comedi);
        return mk_atom(env, "ok");
    }
    return mk_error(env, "error initialising elevator");
}

static ERL_NIF_TERM
setMotorDir(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_is_identical(argv[0], mk_atom(env,"up")))
    {
        elev_set_motor_direction(DIRN_UP);
        return mk_atom(env, "motor going up");
    }

    else if (enif_is_identical(argv[0], mk_atom(env,"down")))
    {
        elev_set_motor_direction(DIRN_DOWN);
        return mk_atom(env, "motor going down");
    }
    else if (enif_is_identical(argv[0], mk_atom(env,"stop")))
    {
        elev_set_motor_direction(DIRN_STOP);
        return mk_atom(env, "motor stopped");
    }
    return mk_error(env, "error setting motor dir");
}

static ERL_NIF_TERM
setLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    int floor, val;
    elev_button_type_t buttonType;
    if (atomToButtonType(env,argv[0],&buttonType) && enif_get_int(env,argv[1], &floor) && atomToBool(env,argv[2], &val))
    {
        elev_set_button_lamp(buttonType,floor,val);
        return mk_atom(env, "ok"); 
    }
    return mk_error(env, "error setting light");
}

static ERL_NIF_TERM
getFloor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 0)
    {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, elev_get_floor_sensor_signal());
}

static ERL_NIF_TERM
getButtonSignal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    int floor;
    elev_button_type_t buttonType;
    if (atomToButtonType(env,argv[0],&buttonType) && enif_get_int(env,argv[1], &floor))
    {
        return enif_make_int(env, elev_get_button_signal(buttonType,floor)); 
    }
    return mk_error(env, "error getting button signal");
}

static ERL_NIF_TERM
setDoorLight(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    int value;
    if (atomToBool(env,argv[0],&value))
    {
        elev_set_door_open_lamp(value);
        return mk_atom(env, "ok");
    }
    return mk_error(env, "error opening door");  
}

static ERL_NIF_TERM
setFloorIndicator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    int floor;
    if (enif_get_int(env,argv[0], &floor))
    {
        elev_set_floor_indicator(floor);
        return mk_atom(env, "ok");
    }
    return mk_error(env, "error setting floor indicator");  
}


static ErlNifFunc nif_funcs[] = {
    {"initElevator", 1 , initElevator},
    {"setMotorDir",1, setMotorDir},
    {"setLight" , 3, setLight},
    {"getFloor", 0, getFloor},
    {"getButtonSignal", 2 , getButtonSignal},
    {"setDoorLight",1,setDoorLight},
    {"setFloorIndicator",1,setFloorIndicator}
};

ERL_NIF_INIT(elevator_driver, nif_funcs, NULL, NULL, NULL, NULL);