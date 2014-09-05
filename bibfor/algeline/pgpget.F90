subroutine pgpget(sd_pgp, param, iobs, lonvec, savejv,&
                  kscal, iscal, rscal, cscal, kvect,&
                  ivect, rvect, cvect)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! Extract the value of a parameter in the temporary data structure for the
! command POST_GENE_PHYS
!
!  sd_pgp [Obl]: Name of the pgp data structure requested [K24]
!  param  [Obl]: Name of the parameter to be extracted [K24]
!  iobs   [Opt]: Index of the observation, by default = 1 [I]
!  lonvec [Opt]: Get the length of the vector [I]
!  savejv [Opt]: Get the corresponding jeveux object name [I]
!  kscal  [Opt]: Value to be extracted in the case of a character parameter [K24]
!  iscal  [Opt]: Value to be extracted in the case of an integer parameter [I]
!  rscal  [Opt]: Value to be extracted in the case of a float parameter [R8]
!  kvect  [Opt]: Vector to be extracted in the case of character parameters [K24]
!  ivect  [Opt]: Vector to be extracted in the case of integer parameters [I]
!  rvect  [Opt]: Vector to be extracted in the case of float parameters [R8]
!
! 1 - First, we verify that the parameter name is valid
! 2 - Second, we extract the value/vector from the correct work vector  
!     according to the sd_pgp data structure's map
!
! Examples : call pgpget('&&OP0058','RESU_IN ',kscal=resu)
!
!            call pgpget('&&OP0058','NOM_CMP ',iobs=2, lonvec=nbcmp)
!            AS_ALLOCATE(vk8=cmp , size=nbcmp)
!            call pgpget('&&OP0058','NOM_CMP ',iobs=2, kvect=cmp)
!
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
!
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=*), intent(in) :: sd_pgp
    character(len=*), intent(in) :: param
    integer, optional, intent(in) :: iobs
    character(len=24), optional, intent(out) :: savejv
    integer, optional, intent(out) :: lonvec
    character(len=*), optional, intent(out) :: kscal
    integer, optional, intent(out) :: iscal
    real(kind=8), optional, intent(out) :: rscal
    complex(kind=8), optional, intent(out) :: cscal
    character(len=*), optional, intent(out) :: kvect(*)
    integer, optional, intent(out) :: ivect(*)
    real(kind=8), optional, intent(out) :: rvect(*)
    complex(kind=8), optional, intent(out) :: cvect(*)
!
!
!   -0.2- Local variables
!   --- For strings copying
    character(len=8) :: sd_pgp_
    character(len=8) :: param_
!
!   --- For general usage
    integer :: nbparams
    parameter (nbparams=24)
!
    aster_logical :: output_test
    integer :: parind(nbparams), ip, i, jvect, jscal, lvec
    character(len=3) :: partyp(nbparams)
    character(len=6) :: k_iobs
    character(len=8) :: params(nbparams)
    character(len=24) :: savename
!
!   -0.3- Initialization
    data  params /'RESU_OUT','RESU_IN ','TYP_RESU','BASE    ','MODELE  ',&
                  'MAILLAGE','NB_OBSER','NOM_CHAM','TYP_CHAM','NOM_CMP ',&
                  'TYP_SCAL','NUM_NOEU','NUM_MAIL','NUM_ORDR','DISC    ',&
                  'ADD_CORR','ACC_MO_A','ACC_DIR ','VEC_PR_R',&
                  'VEC_PR_C','REF_SUP1','REF_SUP2','REF_COMP','REF_INDI'/
!
    data  partyp /'K24','K24','K24','K24','K24',&
                  'K24','I','K24','K24','K24',&
                  'K24','I','I','I','R8', &
                  'I','K24','R8','R8',&
                  'C8','K24','K24','K24','I'/
!
!   parind = -2 : vector global          ; = -1 : scalar global ; 
!          =  2 : vector per observation ; =  1 : scalar per observation
    data  parind  / -1, -1, -1, -1, -1,&
                    -1, -1,  1,  1,  2,&
                     1,  2,  2,  2,  2,&
                     1,  1,  2,  2,&
                     2,  2,  2,  2,  2/
!
    savename = '                        '
!
!   Copying the input strings, in order to allow in-command truncated input
    sd_pgp_ = sd_pgp
    param_ = param
!
    call jemarq()
!
!   ====================================================================
!   = 1 = Validation of the input arguments, distinguishing global vars
!   ====================================================================
!
    if ((.not.present(lonvec)) .and. (.not.present(savejv))) then
        output_test = UN_PARMI4(kscal, iscal, rscal, cscal) .or.&
                      UN_PARMI4(kvect, ivect, rvect, cvect)
!
        ASSERT(output_test)
    end if
!
    do ip = 1, nbparams
        if (params(ip) .eq. param_) goto 10
    end do
 10 continue
!
!   The parameter to be saved was not found in the predefined list
    if (ip .eq. nbparams+1) then
        ASSERT(.false.)
    endif
!
    savename(1:8) = sd_pgp_
    if (present(iobs)) then
!       The parameter to be extracted is global but an observation index was given
        ASSERT(parind(ip).gt.0)
        call codent(iobs, 'G', k_iobs)
        savename(9:15) = '.'//k_iobs(1:6)
    else
        ASSERT(parind(ip).lt.0)
    end if
    savename(16:24)='.'//param_
!
!   ====================================================================
!   = 2 = Extracting data
!   ====================================================================
!
!   --- Length of vectors
    if (present(savejv)) savejv = savename
!
    if (present(lonvec) .or. UN_PARMI4(kscal, iscal, rscal, cscal) .or.&
        UN_PARMI4(kvect, ivect, rvect, cvect)) then
        call jelira(savename, 'LONMAX', lvec)
    end if
!
    if (present(lonvec)) lonvec = lvec
!
    if (UN_PARMI4(kscal, iscal, rscal, cscal) .or. UN_PARMI4(kvect, ivect, rvect, cvect)) then
!
!   --- Vectors
        if (abs(parind(ip)) .eq. 2) then
!
!           The parameter to get is a vector but no vector output was found
            call jeveuo(savename, 'L', jvect)
!
            if (UN_PARMI3(kvect, ivect, rvect)) then
                if (partyp(ip) .eq. 'K24') then
                    do i = 1, lvec
                        kvect(i) = zk24(jvect+i-1)
                    end do
                else if (partyp(ip).eq.'R8') then
                    call dcopy(lvec, zr(jvect), 1, rvect, 1)
                else if (partyp(ip).eq.'C16') then
                    call zcopy(lvec, zc(jvect), 1, cvect, 1)
                else if (partyp(ip).eq.'I') then
                    do i = 1, lvec
                        ivect(i) = zi(jvect+i-1)
                    end do
                end if
            end if 
!
!   --- Scalars
        else if (abs(parind(ip)).eq.1) then
!
!           The parameter to get is a scalar but no scalar output was found
            ASSERT(UN_PARMI3(kscal, iscal, rscal))
!
            call jeveuo(savename, 'L', jscal)
            if (partyp(ip) .eq. 'K24') then
                kscal = zk24(jscal)
            else if (partyp(ip).eq.'R8') then
                rscal = zr(jscal)
            else if (partyp(ip).eq.'C8') then
                cscal = zc(jscal)
            else if (partyp(ip).eq.'I') then
                iscal = zi(jscal)
            end if
!
        end if
    end if
!
!
    call jedema()
!
end subroutine
