subroutine nmdcae(sddisc, iterat, typdec, nbrpas, ratio,&
                  optdec, retdec)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacex.h"
#include "asterfort/nmlerr.h"
#include "asterfort/utmess.h"
    character(len=19) :: sddisc
    integer :: iterat, nbrpas, retdec
    real(kind=8) :: ratio
    character(len=16) :: optdec
    character(len=4) :: typdec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! PARAMETRES DE DECOUPE - EXTRAPOLATION DES RESIDUS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! OUT RATIO  : RATIO DU PREMIER PAS DE TEMPS
! OUT TYPDEC : TYPE DE DECOUPE
!              'SUBD' - SUBDIVISION PAR UN NOMBRE DE PAS DONNE
!              'DELT' - SUBDIVISION PAR UN INCREMENT DONNE
! OUT NBRPAS : NOMBRE DE PAS DE TEMPS
! OUT OPTDEC : OPTION DE DECOUPE
!     'UNIFORME'   - DECOUPE REGULIERE ET UNIFORME
!     'PROGRESSIF' - DECOUPE EN DEUX ZONES, UN PAS LONG+ UNE SERIE
!                    DE PAS UNIFORMES
!     'DEGRESSIF'  - DECOUPE EN DEUX ZONES, UNE SERIE DE PAS
!                    UNIFORMES + UN PAS LONG
! OUT RETDEC : CODE RETOUR DECOUPE
!     0 - ECHEC DE LA DECOUPE
!     1 - ON A DECOUPE
!     2 - PAS DE DECOUPE
!
!
!
!
    real(kind=8) :: un
    logical :: lextra
    real(kind=8) :: valext(4)
    real(kind=8) :: xxbb, xa0, xa1, xdet, cresi, ciblen
    real(kind=8) :: r8bid
    integer :: mniter, mxiter
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    un = 1.d0
    retdec = 0
    ratio = 0.d0
    nbrpas = -1
    optdec = 'UNIFORME'
    typdec = ' '
!
! --- LECTURE DES INFOS SUR LES CONVERGENCES
!
    call nmlerr(sddisc, 'L', 'MNITER', r8bid, mniter)
    call nmlerr(sddisc, 'L', 'MXITER', r8bid, mxiter)
!
! --- EXTRAPOLATION LINEAIRE DES RESIDUS
!
    call nmacex(sddisc, iterat, lextra, valext)
    xa0 = valext(1)
    xa1 = valext(2)
    xdet = valext(3)
    cresi = valext(4)
!
! --- CALCUL DU RATIO
!
    if (.not.lextra) then
        call utmess('I', 'EXTRAPOLATION_10')
        retdec = 0
    else
        call utmess('I', 'EXTRAPOLATION_11')
        nbrpas = 4
        ciblen = (xa0 + xa1*log(cresi) )/xdet
        if (xdet .le. r8prem()) then
            ratio = 24.0d0/((3.0d0*nbrpas+un)**2 - un)
        else
            if ((ciblen*1.20d0) .lt. mniter) then
                ratio = 24.0d0/((3.0d0*nbrpas+un)**2 - un)
            else
                if (xa1 .le. r8prem()) then
                    ratio = 24.0d0/((3.0d0*nbrpas+un)**2 - un)
                else
                    if ((ciblen-mxiter) .le. (-10.0d0*xa1/xdet)) then
                        ratio = exp( (ciblen-mxiter)*xdet/xa1 )
                    else
                        ratio = exp( -10.0d0 )
                    endif
                    ratio = 0.48485d0*ratio
                    xxbb = ( -un + (un+24.0d0/ratio)**0.5d0 )/3.0d0
                    if (xxbb .lt. 2.0d0) then
                        nbrpas = 2
                        ratio = 0.5d0
                    else
                        nbrpas = nint( xxbb )
                    endif
                endif
            endif
        endif
        retdec = 1
        optdec = 'PROGRESSIF'
        typdec = 'SUBD'
    endif
!
    call jedema()
end subroutine
