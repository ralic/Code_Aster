#ifndef MFRONTBEHAVIOUR_H_
#define MFRONTBEHAVIOUR_H_

/* person_in_charge: mathieu.courtois@edf.fr */

#ifdef __cplusplus

#include <string>
#include <vector>

#include "TFEL/System/ExternalLibraryManager.hxx"

/**
 * \brief A simplified wrapper to a MFront behaviour to request
 *        some informations.
 */
class MFrontBehaviour
{
    public:
        MFrontBehaviour(std::string hyp, std::string lib, std::string behav);
        /**
         * \brief Return a vector of the properties names
         */
        std::vector<std::string> getMaterialPropertiesNames();
        ~MFrontBehaviour();

    private:
        //! hypothesis
        const std::string _hypothesis;
        //! library
        const std::string _libname;
        //! behaviour
            const std::string _bname;
        //! names of the material properties
        std::vector<std::string> _mpnames;
        //! indicator to compute properties names only once
        bool _mpnames_computed;

        /**
         * \brief fill the _mpnames attribute
         */
        void fillMaterialPropertiesNames();
};

/**
 * \brief Convert a vector of strings into a char** and size
 * \param[in] svect : vector of strings
 * \param[out] size : number of elements in the vector
 * \return char**
 */
char** vectorOfStringsAsChar(const std::vector<std::string> &, unsigned int *);

extern "C"
{
#endif // __cplusplus

/**
 * \brief Return an array of strings of the properties names
 * @param hypothesis
 * @param library
 * @param function
 * @param size      Size of the array, number of properties
 */
char** getMaterialPropertiesNames(const char*, const char*, const char*,
                                  unsigned int *);

/**
 * \brief Return an array of strings of the properties names
 *        using the default library name and Tridimensional hypothesis
 * @param behaviour name
 * @param size      Size of the array, number of properties
 */
char** getTridimMaterialPropertiesNames(const char*,
                                        unsigned int *);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // MFRONTBEHAVIOUR_H_
