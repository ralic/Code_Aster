#include <string>
#include <vector>
#include <stdexcept>
#include <algorithm>

#include <stdlib.h>
#include <string.h>

#include "asterc_config.h"
#include "MFrontBehaviour.h"

using namespace std;

/* Constructor */
MFrontBehaviour::MFrontBehaviour(string hypo, string lib, string behav) :
    _hypothesis(hypo), _libname(lib), _bname(behav), _mpnames_computed(false)
{
    // empty
}


/* Destructor */
MFrontBehaviour::~MFrontBehaviour()
{
    // empty
}

vector<string> MFrontBehaviour::getMaterialPropertiesNames()
{
    if ( ! _mpnames_computed ) {
        fillMaterialPropertiesNames();
    }
    return _mpnames;
}

/* private members */
void MFrontBehaviour::fillMaterialPropertiesNames()
{
    using namespace tfel::system;
    // using namespace tfel::material;

    ExternalLibraryManager& elm = ExternalLibraryManager::getExternalLibraryManager();
    _mpnames = elm.getUMATMaterialPropertiesNames(_libname, _bname, _hypothesis);
    bool eo = elm.getUMATRequiresStiffnessTensor(_libname, _bname, _hypothesis);
    bool to = elm.getUMATRequiresThermalExpansionCoefficientTensor(_libname, _bname, _hypothesis);
    unsigned short etype = elm.getUMATElasticSymmetryType(_libname, _bname);
    vector<string> tmp;
    if (etype == 0u) {
        if (eo) {
            tmp.push_back("YoungModulus");
            tmp.push_back("PoissonRatio");
        }
        if (to) {
            tmp.push_back("ThermalExpansion");
        }
    }
    else if (etype == 1u) {
        if (_hypothesis == "AxisymmetricalGeneralisedPlaneStrain") {
            if (eo) {
                tmp.push_back("YoungModulus1");
                tmp.push_back("YoungModulus2");
                tmp.push_back("YoungModulus3");
                tmp.push_back("PoissonRatio12");
                tmp.push_back("PoissonRatio23");
                tmp.push_back("PoissonRatio13");
            }
            if (to) {
                tmp.push_back("ThermalExpansion1");
                tmp.push_back("ThermalExpansion2");
                tmp.push_back("ThermalExpansion3");
            }
        }
        else if ((_hypothesis == "PlaneStress")||
                 (_hypothesis == "PlaneStrain")||
                 (_hypothesis == "Axisymmetrical")||
                 (_hypothesis == "GeneralisedPlaneStrain")) {
            if (eo) {
                tmp.push_back("YoungModulus1");
                tmp.push_back("YoungModulus2");
                tmp.push_back("YoungModulus3");
                tmp.push_back("PoissonRatio12");
                tmp.push_back("PoissonRatio23");
                tmp.push_back("PoissonRatio13");
                tmp.push_back("ShearModulus12");
            }
            if (to) {
                tmp.push_back("ThermalExpansion1");
                tmp.push_back("ThermalExpansion2");
                tmp.push_back("ThermalExpansion3");
            }
        }
        else if (_hypothesis == "Tridimensional") {
            if (eo) {
                tmp.push_back("YoungModulus1");
                tmp.push_back("YoungModulus2");
                tmp.push_back("YoungModulus3");
                tmp.push_back("PoissonRatio12");
                tmp.push_back("PoissonRatio23");
                tmp.push_back("PoissonRatio13");
                tmp.push_back("ShearModulus12");
                tmp.push_back("ShearModulus23");
                tmp.push_back("ShearModulus13");
            }
            if (to) {
                tmp.push_back("ThermalExpansion1");
                tmp.push_back("ThermalExpansion2");
                tmp.push_back("ThermalExpansion3");
            }
        }
        else {
            string msg("MFrontBehaviour::fillMaterialPropertiesNames : "
                       "unsupported modelling hypothesis");
            throw(runtime_error(msg));
        }
    }
    else {
        string msg("MFrontBehaviour::fillMaterialPropertiesNames : "
                   "unsupported behaviour type "
                   "(neither isotropic nor orthotropic)");
        throw(runtime_error(msg));
    }
    _mpnames.insert(_mpnames.begin(), tmp.begin(), tmp.end());
    _mpnames_computed = true;
}

string toAsterParameter(const string &param)
{
    string buff;
    for (string::const_iterator it = param.begin(); it != param.end(); ++it) {
        if ( *it == '[') {
            buff.push_back('_');
        } else if ( *it != ']') {
            buff.push_back(*it);
        }
    }
    return buff;
}

vector<string> toAsterParameterVect(const vector<string> &svect)
{
    vector<string> res;
    for (vector<string>::const_iterator it = svect.begin(); it != svect.end(); ++it) {
        res.push_back(toAsterParameter(*it));
    }
    return res;
}

char** vectorOfStringsAsCharArray(const vector<string> &svect, unsigned int *size)
{
    char **res;
    *size = (unsigned int)(svect.size());
    res = (char **)malloc((size_t)*size * sizeof(char*));
    for (unsigned int i = 0; i < *size; ++i) {
        res[i] = (char *)malloc( ((size_t)svect[i].size() + 1) * sizeof(char));
        strcpy(res[i], svect[i].c_str());
    }
    return res;
}

/* for a simple C access */
char** getMaterialPropertiesNames(const char* hyp, const char* lib, const char* funct,
                                  unsigned int* size)
{
    char **res;

    MFrontBehaviour behaviour(hyp, lib, funct);
    vector<string> names = behaviour.getMaterialPropertiesNames();
    vector<string> conv = toAsterParameterVect(names);
    res = vectorOfStringsAsCharArray(conv, size);
    return res;
}

char** getTridimMaterialPropertiesNames(const char* behav,
                                        unsigned int *size)
{
    char **res;
    string bname(behav);
    transform(bname.begin(), bname.end(), bname.begin(), ::tolower);
    bname.insert(0, "aster");

    MFrontBehaviour behaviour("Tridimensional",
                              "lib" + string(ASTERBEHAVIOUR) + ".so",
                              bname.c_str());
    vector<string> names = behaviour.getMaterialPropertiesNames();
    vector<string> conv = toAsterParameterVect(names);
    res = vectorOfStringsAsCharArray(conv, size);
    return res;
}
