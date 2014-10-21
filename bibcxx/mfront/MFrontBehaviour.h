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

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // MFRONTBEHAVIOUR_H_
