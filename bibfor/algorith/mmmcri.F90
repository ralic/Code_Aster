subroutine mmmcri(criter, noma, depmoi, depgeo, depplu,&
                  resoco, epsmax, cvgnoe, cvgval, mmconv)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cnomax.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/vtaxpy.h"
    character(len=4) :: criter
    character(len=8) :: noma
    character(len=19) :: depmoi
    character(len=19) :: depgeo
    character(len=19) :: depplu
    character(len=24) :: resoco
    real(kind=8) :: epsmax
    logical :: mmconv
    character(len=16) :: cvgnoe
    real(kind=8) :: cvgval
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - ALGORITHME - UTILITAIRE)
!
! CALCUL DU CRITERE DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  CRITER : 'GEOM' OU 'FROT'
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEPMOI : CHAMP GEOMETRIQUE A L'ITERATION GEOM. N-2
! IN  DEPGEO : CHAMP GEOMETRIQUE A L'ITERATION GEOM. N-1
! IN  DEPPLU : CHAMP GEOMETRIQUE A L'ITERATION GEOM. N
! IN  EPSMAX : VALEUR DU CRITERE DE CONVERGENCE
! OUT CVGNOE : LIEU OU LE CRITERE EST MAX
! OUT CVGVAL : VALEUR DU CRITERE MAX
! OUT MMCONV : VAUT .TRUE. SI LE CRITERE EST OK
!
!
!
!
    real(kind=8) :: vmax1, vmax2, vmax3, vmax4, vmaxi, rmin
    real(kind=8) :: cridep, crilbd, alpha
    character(len=24) :: vtdiff, vtdif2
    character(len=8) :: licmp(3), nomnoe
    integer :: ncmp, numno1, numno2, numno3, numno4, numnoe
    logical :: mmcvge, mmcvfr
    character(len=24) :: maxdep
    integer :: jmaxde
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CALCUL DU CRITERE (DEPLACEMENT RELATIF)
!
    mmconv = .false.
!
! --- INITIALISATIONS
!
    vmax1 = 0.d0
    vmax2 = 0.d0
    vmax3 = 0.d0
    vmax4 = 0.d0
    cridep = 0.d0
    crilbd = 0.d0
    cvgnoe = ' '
    cvgval = r8vide()
    alpha = -1.d0
    mmcvge = .false.
    mmcvfr = .false.
!
! --- CONSTRUCTION CHAM_NO DEPPLU-DEPGEO
!
    vtdiff = '&&MMMCRI.VTDIFF'
    call copisd('CHAMP_GD', 'V', depplu, vtdiff)
    call vtaxpy(alpha, depgeo, vtdiff)
!
    if (criter .eq. 'GEOM') then
!
! --- LA REFERENCE POUR LA BOUCLE DE GEOMETRIE EST DEPGEO-DEPMOI
!
        vtdif2 = '&&MMMCRI.VTDIF2'
        call copisd('CHAMP_GD', 'V', depgeo, vtdif2)
        call vtaxpy(alpha, depmoi, vtdif2)
!
! --- VARIATION RELATIVE DE DEPPLU-DEPGEO SUR LES DEPLACEMENTS
!                           DEPGEO-DEPMOI
!
        ncmp = 3
        licmp(1) = 'DX'
        licmp(2) = 'DY'
        licmp(3) = 'DZ'
!
        call cnomax(vtdiff, ncmp, licmp, vmax1, numno1)
        call cnomax(vtdif2, ncmp, licmp, vmax2, numno2)
!
! --- STOCKAGE DU MAX DE LA NORME DU DEPLACEMENT
!
        maxdep = resoco(1:14)//'.MAXD'
        call jeveuo(maxdep, 'E', jmaxde)
        if (zr(jmaxde) .lt. 0.d0) then
            zr(jmaxde-1+1) = vmax2
            rmin = r8prem()
        else
            zr(jmaxde-1+1) = max(zr(jmaxde-1+1),vmax2)
            rmin = 1.d-6*zr(jmaxde-1+1)
        endif
!
        if (vmax2 .le. rmin) then
            if (vmax2 .eq. 0.d0) then
                cridep = 10.0d0*epsmax
            else
                cridep = 1.d-1*epsmax
            endif
        else
            cridep = vmax1/vmax2
        endif
!
! --- TEST DE CONVERGENCE
!
        if (cridep .lt. abs(epsmax)) then
            mmcvge = .true.
        else
            mmcvge = .false.
        endif
!
    else if (criter.eq.'FROT') then
!
!
! --- VARIATION RELATIVE DE DEPPLU-DEPGEO SUR LES LAGS_C
!
        ncmp = 1
        licmp(1) = 'LAGS_C'
        licmp(2) = ' '
        licmp(3) = ' '
!
        call cnomax(vtdiff, ncmp, licmp(1), vmax3, numno3)
        call cnomax(depplu, ncmp, licmp(1), vmax4, numno4)
!
        if (vmax4 .gt. 0.d0) then
            crilbd = vmax3/vmax4
        else
            crilbd = 0.d0
        endif
!
! --- TEST DE CONVERGENCE
!
        if (crilbd .lt. abs(epsmax)) then
            mmcvfr = .true.
        else
            mmcvfr = .false.
        endif
!
    else
        call assert(.false.)
    endif
!
!
! --- CONVERGENCE FINALE
!
    mmconv = mmcvge.or.mmcvfr
!
! --- INFORMATIONS: NOM DU NOEUD ET VALEUR REACTUALISATION
!
    if (criter .eq. 'FROT') then
        numnoe = numno3
        vmaxi = crilbd
    else
        numnoe = numno1
        vmaxi = cridep
    endif
    if (numnoe .eq. 0) then
        nomnoe = ' '
    else
        call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
    endif
    cvgnoe = nomnoe//'        '
    cvgval = vmaxi
!
    call detrsd('CHAMP_GD', vtdiff)
    if (criter .eq. 'GEOM') then
        call detrsd('CHAMP_GD', vtdif2)
    endif
!
    call jedema()
end subroutine
