subroutine te0360(option, nomte)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/eiangl.h"
#include "asterfort/eifint.h"
#include "asterfort/eiinit.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          POUR LES ELEMENTS D'INTERFACE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: typmod(2), nomail, lielrf(10)
    logical :: axi
    integer :: nno1, nno2, npg, imatuu, lgpg, lgpg1, lgpg2
    integer :: iw, ivf1, idf1, igeom, imate
    integer :: ivf2, idf2, nnos, jgn
    integer :: ivarim, ivarip, iinstm, iinstp
    integer :: iddlm, iddld, icompo, icarcr, icamas
    integer :: ivectu, icontp
    integer :: ivarix
    integer :: jtab(7), iadzi, iazk24, jcret, codret
    integer :: ndim, iret, ntrou, vali(2)
    integer :: iu(3, 18), im(3, 9), it(18)
    real(kind=8) :: ang(24)
!
!
!
! - FONCTIONS DE FORME
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elref4(lielrf(1), 'RIGI', ndim, nno1, nnos,&
                npg, iw, ivf1, idf1, jgn)
    call elref4(lielrf(2), 'RIGI', ndim, nno2, nnos,&
                npg, iw, ivf2, idf2, jgn)
    ndim = ndim + 1
    axi = lteatt('AXIS','OUI')
!
! - DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
    call eiinit(nomte, iu, im, it)
!
! - TYPE DE MODELISATION
!
    if (ndim .eq. 3) then
        typmod(1) = '3D'
    else if (axi) then
        typmod(1) = 'AXIS'
    else
        typmod(1) = 'PLAN'
    endif
!
    typmod(2) = 'INTERFAC'
    codret = 0
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDEPLMR', 'L', iddlm)
    call jevech('PDEPLPR', 'L', iddld)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
!
! --- ORIENTATION DE L'ELEMENT D'INTERFACE : REPERE LOCAL
!     RECUPERATION DES ANGLES NAUTIQUES DEFINIS PAR AFFE_CARA_ELEM
!
    call jevech('PCAMASS', 'L', icamas)
    if (zr(icamas) .eq. -1.d0) then
        call utmess('F', 'ELEMENTS5_47')
    endif
!
!     DEFINITION DES ANGLES NAUTIQUES AUX NOEUDS SOMMETS : ANG
!
    call eiangl(ndim, nno2, zr(icamas+1), ang)
!
!
! - ON VERIFIE QUE PVARIMR ET PVARIPR ONT LE MEME NOMBRE DE V.I. :
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg1 = max(jtab(6),1)*jtab(7)
!
    if ((option(1:4).eq.'RAPH') .or. (option(1:4).eq.'FULL')) then
        call tecach('OON', 'PVARIPR', 'E', iret, nval=7,&
                    itab=jtab)
        lgpg2 = max(jtab(6),1)*jtab(7)
!
        if (lgpg1 .ne. lgpg2) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3) (1:8)
            vali(1)=lgpg1
            vali(2)=lgpg2
            call utmess('A', 'CALCULEL6_64', sk=nomail, ni=2, vali=vali)
        endif
    endif
    lgpg = lgpg1
!
!
! - VARIABLES DE COMMANDE
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!
! PARAMETRES EN SORTIE
!
    if (option(1:4) .eq. 'RIGI' .or. option(1:4) .eq. 'FULL') then
        call jevech('PMATUUR', 'E', imatuu)
    else
        imatuu=1
    endif
!
    if (option(1:4) .eq. 'RAPH' .or. option(1:4) .eq. 'FULL') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
!
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
        ivectu=1
        icontp=1
        ivarip=1
    endif
!
!
! - FORCES INTERIEURES ET MATRICE TANGENTE
!
    if (zk16(icompo+2)(1:5) .eq. 'PETIT') then
!
        call eifint(ndim, axi, nno1, nno2, npg,&
                    zr(iw), zr(ivf1), zr(ivf2), zr(idf2), zr(igeom),&
                    ang, typmod, option, zi(imate), zk16(icompo),&
                    lgpg, zr(icarcr), zr(iinstm), zr(iinstp), zr(iddlm),&
                    zr(iddld), iu, im, zr(ivarim), zr(icontp),&
                    zr(ivarip), zr(imatuu), zr(ivectu), codret)
!
    else
        call utmess('F', 'ALGORITH17_2', sk=zk16(icompo+2))
    endif
!
    if (option(1:4) .eq. 'FULL' .or. option(1:4) .eq. 'RAPH') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
