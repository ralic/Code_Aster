subroutine mtdorc(modelz, compoz, carcri)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/alcart.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
    character(len=*) :: modelz, compoz
    character(len=24) :: carcri
! ----------------------------------------------------------------------
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
!     SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT UTILISEE
!     POUR CALC_META
! IN  MODELZ  : NOM DU MODELE
! OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! OUT CARCRI  : CARTE DECRIVANT LES CRITERES LOCAUX DE CONVERGENCE
!                     0 : ITER_INTE_MAXI
!                     1 : COMPOSANTE INUTILISEE
!                     2 : RESI_INTE_RELA
!                     3 : THETA
!                     4 : ITER_INTE_PAS
!                     5 : ALGO_INTE
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!
    integer :: ncmpma, dimaki, ibid, nbocc, i, icmp, jma, jncmp
    integer :: jnoma, jvalv, k, n1, nbma, nbmo1, nbvari
    integer :: dimanv
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter (dimanv=4)
    parameter (ncmpma=7+dimaki+dimanv)
    character(len=6) :: nompro
    parameter (nompro='MTDORC')
    character(len=8) :: noma, nomgrd, nomcmp(ncmpma), k8b, typmcl(2)
    character(len=16) :: comp, moclef(2), k16bid, nomcmd, mocles(2)
    character(len=19) :: compor
    character(len=24) :: ligrmo, modele, mesmai
    integer :: iarg
!
    data nomgrd/'COMPOR  '/
    data nomcmp/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',&
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',&
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',&
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
!     ------------------------------------------------------------------
    call jemarq()
!     initialisations
    modele = modelz
!
    call getres(k8b, k16bid, nomcmd)
!
    compor = '&&'//nompro//'.COMPOR'
    nbmo1 = 1
    moclef(1) = 'COMP_INCR'
!
    mocles(1) = 'GROUP_MA'
    mocles(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&'//nompro//'.MES_MAILLES'
!
    ligrmo = modele(1:8)//'.MODELE'
    call jeveuo(ligrmo(1:19)//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
! ======================================================================
!                       REMPLISSAGE DE LA CARTE COMPOR :
! --- ON STOCKE LE NOMBRE DE VARIABLES INTERNES PAR RELATION -----------
! --- DE COMPORTEMENT --------------------------------------------------
! ======================================================================
!
    call alcart('V', compor, noma, nomgrd)
    call jeveuo(compor//'.NCMP', 'E', jncmp)
    call jeveuo(compor//'.VALV', 'E', jvalv)
    do 90 icmp = 1, ncmpma
        zk8(jncmp+icmp-1) = nomcmp(icmp)
90  end do
!
!     mots cles facteur
    do 160 i = 1, nbmo1
        call getfac(moclef(i), nbocc)
!
!       nombre d'occurrences
        do 150 k = 1, nbocc
!
            call getvtx(moclef(i), 'RELATION', k, iarg, 1,&
                        comp, n1)
            call getvis(moclef(i), comp, k, iarg, 1,&
                        nbvari, n1)
            zk16(jvalv-1+1) = comp
            write (zk16(jvalv-1+2),'(I16)') nbvari
            zk16(jvalv-1+3) = ' '
            zk16(jvalv-1+4) = moclef(i)
            zk16(jvalv-1+5) = ' '
            call reliem(modele, noma, 'NU_MAILLE', moclef(i), k,&
                        2, mocles, typmcl, mesmai, nbma)
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jma)
                call nocart(compor, 3, k8b, 'NUM', nbma,&
                            k8b, zi(jma), ' ', ncmpma)
                call jedetr(mesmai)
            else
! -----   PAR DEFAUT C'EST TOUT='OUI'
                call nocart(compor, 1, k8b, k8b, 0,&
                            k8b, ibid, k8b, ncmpma)
            endif
150      continue
160  end do
!
    call jedetr(compor//'.NCMP')
    call jedetr(compor//'.VALV')
    compoz = compor
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
