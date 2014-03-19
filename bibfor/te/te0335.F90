subroutine te0335(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fgequi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tecach.h"
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       CALCULER LES GRANDEURS EQUIVALENTES SUIVANTES
!       . CONTRAINTES EQUIVALENTES               (= 17 VALEURS)
!          . VON MISES                               (= 1 VALEUR)
!          . TRESCA                                  (= 1 VALEUR)
!          . CONTRAINTES PRINCIPALES                 (= 3 VALEURS)
!          . VON-MISES * SIGNE (PRESSION)            (= 1 VALEUR)
!          . DIRECTIONS DES CONTRAINTES PRINCIPALES  (= 3*3 VALEURS)
!          . TRACE                                   (= 1 VALEUR)
!          . TAUX DE TRIAXIALITE                     (= 1 VALEUR)
!
!       . DEFORMATIONS EQUIVALENTES              (= 14 VALEURS)
!          . SECOND INVARIANT                        (= 1 VALEUR)
!          . DEFORMATIONS PRINCIPALES                (= 3 VALEURS)
!          . 2EME INV. * SIGNE (1ER.INV.)            (= 1 VALEUR)
!          . DIRECTIONS DES DEFORMATIONS PRINCIPALES (= 3*3 VALEURS)
!
!       AUX POINTS DE GAUSS ET AUX NOEUDS :
!       A PARTIR DE SIGM_ELGA ET SIGM_ELNO POUR LES CONTRAINTES
!       A PARTIR DE EPSI_ELGA ET EPSI_ELNO POUR LES DEFORMATIONS
!       A PARTIR DE EPME_ELGA ET EPME_ELNO POUR LES DEF. HORS THERMIQUE
!
!       OPTION : 'SIEQ_ELGA'
!                'SIEQ_ELNO'
!                'EPEQ_ELGA'
!                'EPEQ_ELNO'
!                'EPMQ_ELGA'
!                'EPMQ_ELNO'
!
! ----------------------------------------------------------------------
!
!
!
    integer :: neeqmx, nceqmx
    parameter (neeqmx=14,nceqmx=17)
!
    integer :: ndim, ndim1, nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: idefo, icont, iequi
    integer :: iret, itabin(7), itabou(7), nbcmp, ncmpeq, nbsp
    integer :: idec, ideceq, ipg, ino, isp
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if ((nomte.eq.'MEC3QU9H') .or. (nomte.eq.'MEC3TR7H')) then
        call elrefe_info(fami='MASS',ndim=ndim1,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    else
        call elrefe_info(fami='RIGI',ndim=ndim1,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
    endif
!
    if ((option.eq.'EPEQ_ELGA') .or. (option.eq.'EPEQ_ELNO') .or. (option.eq.'EPMQ_ELGA')&
        .or. (option.eq.'EPMQ_ELNO')) then
!
        call tecach('OOO', 'PDEFORR', 'L', iret, nval=7,&
                    itab=itabin)
        idefo=itabin(1)
        call tecach('OOO', 'PDEFOEQ', 'E', iret, nval=7,&
                    itab=itabou)
        ASSERT(itabou(2)/itabou(3).eq.neeqmx)
!
        elseif ((option.eq.'SIEQ_ELGA') .or. (option.eq.'SIEQ_ELNO'))&
    then
!
        call tecach('OOO', 'PCONTRR', 'L', iret, nval=7,&
                    itab=itabin)
        icont=itabin(1)
        call tecach('OOO', 'PCONTEQ', 'E', iret, nval=7,&
                    itab=itabou)
        ASSERT(itabou(2)/itabou(3).eq.nceqmx)
!
    else
        ASSERT(.false.)
    endif
!
    iequi = itabou(1)
!
    nbsp = itabou(7)
    ASSERT(nbsp.ge.1)
    ASSERT(nbsp.eq.itabin(7))
!
    nbcmp = itabin(2)/itabin(3)
    ASSERT((nbcmp.eq.1).or.(nbcmp.eq.4).or.(nbcmp.eq.6))
!
    ncmpeq = itabou(2)/itabou(3)
    ASSERT((ncmpeq.eq.neeqmx).or.(ncmpeq.eq.nceqmx))
!
    ASSERT(itabin(6).le.1)
    ASSERT(itabou(6).le.1)
!
    if (nbcmp .eq. 6) then
        ndim = 3
    else if (nbcmp.eq.4) then
        ndim = 2
    else if (nbcmp.eq.1) then
        ndim = 1
    endif
!
! ----------------------------------------------------------------
! --- DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS
! ----------------------------------------------------------------
!
    if (option(6:9) .eq. 'ELGA') then
!
! ------ DEFORMATIONS :
! -------------------
        if ((option.eq.'EPEQ_ELGA') .or. (option.eq.'EPMQ_ELGA')) then
            do 10 ipg = 1, npg
                do 11 isp = 1, nbsp
                    idec = idefo+(ipg-1)*nbcmp *nbsp+(isp-1)*nbcmp
                    ideceq = iequi+(ipg-1)*ncmpeq*nbsp+(isp-1)*ncmpeq
                    call fgequi(zr(idec), 'EPSI_DIR', ndim, zr(ideceq))
11              continue
10          continue
!
! ----- CONTRAINTES :
! -----------------
        else if (option.eq.'SIEQ_ELGA') then
            do 20 ipg = 1, npg
                do 21 isp = 1, nbsp
                    idec = icont+(ipg-1)*nbcmp *nbsp+(isp-1)*nbcmp
                    ideceq = iequi+(ipg-1)*ncmpeq*nbsp+(isp-1)*ncmpeq
                    call fgequi(zr(idec), 'SIGM_DIR', ndim, zr(ideceq))
21              continue
20          continue
        endif
!
! -------------------------------------------------------
! --- DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX NOEUDS
! -------------------------------------------------------
!
    else if (option(6:9).eq.'ELNO') then
!
! ------ DEFORMATIONS :
! -------------------
        if ((option.eq.'EPEQ_ELNO') .or. (option.eq.'EPMQ_ELNO')) then
            do 30 ino = 1, nno
                do 31 isp = 1, nbsp
                    idec = idefo+(ino-1)*nbcmp *nbsp+(isp-1)*nbcmp
                    ideceq = iequi+(ino-1)*ncmpeq*nbsp+(isp-1)*ncmpeq
                    call fgequi(zr(idec), 'EPSI_DIR', ndim, zr(ideceq))
31              continue
30          continue
!
! ----- CONTRAINTES :
! -----------------
        else if (option.eq.'SIEQ_ELNO') then
            do 40 ino = 1, nno
                do 41 isp = 1, nbsp
                    idec = icont+(ino-1)*nbcmp *nbsp+(isp-1)*nbcmp
                    ideceq = iequi+(ino-1)*ncmpeq*nbsp+(isp-1)*ncmpeq
                    call fgequi(zr(idec), 'SIGM_DIR', ndim, zr(ideceq))
41              continue
40          continue
        endif
!
    endif
!
    call jedema()
!
end subroutine
