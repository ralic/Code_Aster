subroutine xpraju(noma, fiss, cnslt, cnsvt, cnsvn,&
                  deltat, vmax)
!
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: noma, fiss
    character(len=19) :: cnsvt, cnsvn, cnslt
    real(kind=8) :: deltat, vmax
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRAJU   : X-FEM PROPAGATION : AJUSTEMENT DE VN
!       ------     -     --            ---
!    AJUSTEMENT DU CHAMP DE VITESSE VN :
!          SI  LT <=0 , VN AJUSTEE = 0
!          SINON, VN AJUSTEE = (VN*LST)/(VT*DELTAT)
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE X-FEM DE LA FISSURE A PROPAGER
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTIELLE
!        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
!        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
!        DELTAT  : TEMPS TOTAL DE PROPAGATION
!
!    SORTIE
!        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION AJUSTEE
!        VMAX    : VALEUR MAXIMALE DES COMPOSANTES DE VITESSE
!
!     ------------------------------------------------------------------
!
!
    integer :: i, nbno,    ifm, niv, cptzo, cptaju
    integer :: jlisno
    real(kind=8) :: modzon, dmin
    real(kind=8), pointer :: ltno(:) => null()
    real(kind=8), pointer :: vnno(:) => null()
    real(kind=8), pointer :: vtno(:) => null()
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!  RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
!   RECUPERATION DES ADRESSES DES CHAMPS DE VITESSE AUX NOEUDS
    call jeveuo(cnsvt//'.CNSV', 'L', vr=vtno)
    call jeveuo(cnsvn//'.CNSV', 'E', vr=vnno)
!
!   RECUPERATION DE L'ADRESSE DES VALEURS DE LST
    call jeveuo(cnslt//'.CNSV', 'L', vr=ltno)
!
!   RETRIEVE THE LIST OF THE NODES THAT MUST TO BE USED IN THE
!   CALCULUS
    call jeveuo(fiss//'.PRO.NOEUD_TORE', 'L', jlisno)
!
    cptzo = 0
    cptaju = 0
    dmin = r8miem()
    vmax = 0.d0
!
!   BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE
    do i = 1, nbno
!
        if (zl(jlisno-1+i)) then
!
            if (ltno(i) .le. dmin) then
!
!             THE NODE (OR ITS PROJECTION) IS ON THE EXISTING CRACK
!             SURFACE. ITS NORMAL SPEED MUST BE SET TO ZERO.
                vnno(i) = 0
!
!             CALCULATE THE MAXIMUM VALUE OF THE SPEED COMPONENTS
                if (abs(vtno(i)) .gt. vmax) vmax=abs(vtno(i) )
!
                cptzo = cptzo+1
!
            else
!
!             THE NODE (OR ITS PROJECTION) IS AHEAD OF THE CRACK TIP.
!             ITS NORMAL SPEED MUST BE RECALCULATED USING A LINEAR
!             EXTRAPOLATION.
                modzon = vtno(i)*deltat
                vnno(i) = vnno(i)*ltno(i)/modzon
!
!             CALCULATE THE MAXIMUM VALUE OF THE SPEED COMPONENTS
                if (abs(vtno(i)) .gt. vmax) vmax=abs(vtno(i) )
                if (abs(vnno(i)) .gt. vmax) vmax=abs(vnno(i) )
!
                cptaju = cptaju+1
!
            endif
!
        endif
!
    end do
!
    if (niv .ge. 1) then
        write(ifm,*)'   NOMBRE DE NOEUDS DONT VN EST ANNULEE :',cptzo
        write(ifm,*)'   NOMBRE DE NOEUDS DONT VN EST AJUSTEE :',cptaju
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
