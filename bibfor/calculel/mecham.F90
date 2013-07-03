subroutine mecham(option, modele, cara, nh, chgeoz,&
                  chcara, chharz, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/meharm.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: iret, nh
    character(len=*) :: option, modele, cara
    character(len=*) :: chgeoz, chcara(*), chharz
!
!     ------------------------------------------------------------------
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
!     VERIFICATION DES CHAMPS DONNES :
!        - ON VERIFIE LES EVENTUELLES SOUS-STRUCTURES STATIQUES
!        - ON VERIFIE S'IL Y A 1 LIGREL DANS LE MODELE
!     ------------------------------------------------------------------
! IN  : OPTION : OPTION DE CALCUL
! IN  : MODELE : MODELE
! IN  : CARA   : CHAMP DE CARA_ELEM
! IN  : NH     : NUMERO D'HARMONIQUE DE FOURIER
! OUT : CHGEOZ : NOM DE CHAMP DE GEOMETRIE TROUVE
! OUT : CHCARA : NOMS DES CHAMPS DE CARACTERISTIQUES TROUVES
! OUT : CHHARZ : NOM DU CHAMP D'HARMONIQUE DE FOURIER TROUVE
! OUT : IRET  : CODE RETOUR
!                = 0 : LE MODELE CONTIENT DES ELEMENTS FINIS
!                = 1 : LE MODELE NE CONTIENT PAS D'ELEMENTS FINIS
!     ------------------------------------------------------------------
    character(len=8) :: k8b, nomo, noma, nomacr, exiele
    character(len=24) :: chgeom, chharm
    logical :: exicar
!
!-----------------------------------------------------------------------
    integer :: ianmcr, iasssa, ibid, ier, ima, iexi, nbsma
    integer :: nbss
!-----------------------------------------------------------------------
    call jemarq()
    chgeom=' '
    chharm=' '
!
    call assert(modele(1:1).ne.' ')
    nomo=modele
!
!     --- ON VERIFIE LES EVENTUELLES SOUS-STRUCTURES STATIQUES:
    call dismoi('F', 'NB_SS_ACTI', nomo, 'MODELE', nbss,&
                k8b, ier)
    if (nbss .gt. 0) then
        call dismoi('F', 'NB_SM_MAILLA', nomo, 'MODELE', nbsma,&
                    k8b, ier)
        call dismoi('F', 'NOM_MAILLA', nomo, 'MODELE', ibid,&
                    noma, ier)
        call jeveuo(noma//'.NOMACR', 'L', ianmcr)
        call jeveuo(nomo//'.MODELE    .SSSA', 'L', iasssa)
!
!        --- BOUCLE SUR LES (SUPER)MAILLES ---
        ier=0
        if (option(1:9) .eq. 'MASS_MECA') then
            do 10 ima = 1, nbsma
                if (zi(iasssa-1+ima) .eq. 1) then
                    nomacr=zk8(ianmcr-1+ima)
                    call jeexin(nomacr//'.MAEL_MASS_VALE', iexi)
                    if (iexi .eq. 0) then
                        ier=ier+1
                        call u2mesk('E', 'CALCULEL3_31', 1, nomacr)
                    endif
                endif
10          continue
            if (ier .gt. 0) call u2mess('F', 'CALCULEL3_32')
        else if (option(1:9).eq.'RIGI_MECA') then
            do 20 ima = 1, nbsma
                if (zi(iasssa-1+ima) .eq. 1) then
                    nomacr=zk8(ianmcr-1+ima)
                    call jeexin(nomacr//'.MAEL_RAID_VALE', iexi)
                    if (iexi .eq. 0) then
                        ier=ier+1
                        call u2mesk('E', 'CALCULEL3_33', 1, nomacr)
                    endif
                endif
20          continue
            if (ier .gt. 0) then
                call u2mess('F', 'CALCULEL3_34')
            endif
        else if (option(1:9).eq.'AMOR_MECA') then
            do 30 ima = 1, nbsma
                if (zi(iasssa-1+ima) .eq. 1) then
                    nomacr=zk8(ianmcr-1+ima)
                    call jeexin(nomacr//'.MAEL_AMOR_VALE', iexi)
                    if (iexi .eq. 0) then
                        ier=ier+1
                        call u2mesk('E', 'CALCULEL6_80', 1, nomacr)
                    endif
                endif
30          continue
            if (ier .gt. 0) call u2mess('F', 'CALCULEL6_81')
        endif
    endif
!
!     --- ON REGARDE S'IL Y A 1 LIGREL DANS LE MODELE ---
    call dismoi('F', 'EXI_ELEM', nomo, 'MODELE', ibid,&
                exiele, ier)
    if (exiele(1:3) .eq. 'OUI') then
        iret=0
    else
        iret=1
    endif
    if (iret .eq. 1 .and. nbss .eq. 0) call u2mess('F', 'CALCULEL3_35')
!
!     --- SI IL N'Y A PAS D'ELEMENTS, ON SORT :
    if (iret .eq. 1) goto 40
!
    call megeom(nomo, chgeom)
    call mecara(cara, exicar, chcara)
!     --- ON CREE UN CHAMP D'HARMONIQUE DE FOURIER (CARTE CSTE) ---
    call meharm(nomo, nh, chharm)
!
40  continue
    chgeoz=chgeom
    chharz=chharm
    call jedema()
end subroutine
