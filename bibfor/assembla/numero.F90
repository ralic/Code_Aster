subroutine numero(nuposs, modelz, infchz, solveu, base,&
                  nu)
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
! ----------------------------------------------------------------------
! IN  K14  NUPOSS  : NOM D'UN NUME_DDL CANDIDAT (OU ' ')
!                    SI NUPOSS != ' ', ON  REGARDE SI LE PROF_CHNO
!                    DE NUPOSS EST CONVENABLE.
!                    (POUR EVITER DE CREER SYTEMATIQUEMENT 1 PROF_CHNO)
! IN  K8   MODELE  : NOM DU MODELE
! IN  K19  INFCHA  : NOM DE L'OBJET DE TYPE INFCHA
! IN  K19  SOLVEU  : NOM DE L'OBJET DE TYPE SOLVEUR
! IN  K2   BASE    : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                  : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
! VAR/JXOUT K14 NU : NOM DU NUME_DDL.
!                    SI NUPOSS !=' ', NU PEUT ETRE MODIFIE (NU=NUPOSS)
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/gcncon.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/numer2.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: modelz, solveu, infchz
    character(len=*) :: nu, nuposs
    character(len=2) :: base
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nchar, nblig, iret, jchar, jlligr, k, jtypch, islvk, idime, i
    integer :: ilima, nbma, nbsd, ifm, niv, ibid, isolfs, irefe, ideeq, ifetn
    integer :: nequa, nbpb, ncharf, l, ivligr, inueq, ifel1, ldeeqg, iinf, ifcpu
    integer :: idd, jmult, ier, nbproc, rang, ilimpi, nivmpi, nbchat, iffcc
    integer :: nequag, nbi2, iaux, ino, icmp, imult, vali(2)
    real(kind=8) :: temps(6), rbid
    character(len=1) :: k1
    character(len=3) :: verif
    character(len=8) :: moloc, nomcha, k8bid, method, nomsd, modele
    character(len=14) :: nuposb, nomfe2
    character(len=16) :: pheno
    character(len=19) :: infcha, ligrsd
    character(len=24) :: lcharg, lligr, nomlig, nomsda, k24b
    character(len=24) :: ksolvf, lligrs, noobj, k24mul
    logical :: lcf
!
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!-----------------------------------------------------------------------
! CONSTRUCTION D'UN OBJET JEVEUX CONTENANT LA LISTE DES CHARGES ET
! LE NOM DU MODELE DE CALCUL
!-----------------------------------------------------------------------
    call jemarq()
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.2', 'DEBUT', ' ')
!
!
    infcha = infchz
    modele = modelz
    lcharg = infcha//'.LCHA'
    nchar = 0
    call jeexin(lcharg, iret)
    if (iret .ne. 0) then
        call jelira(lcharg, 'LONMAX', nchar)
        call jeveuo(lcharg, 'L', jchar)
    endif
    lligr = '&&NUMERO.LISTE_LIGREL'
!
!     LISTE
    call wkvect(lligr, 'V V K24', nchar+1, jlligr)
    nblig = 0
!     ON INSERE LE LIGREL DE MODELE
    call jeexin(modele//'.MODELE    .NBNO', iret)
    if (iret .gt. 0) then
        zk24(jlligr) = modele(1:8)//'.MODELE'
        nblig = nblig + 1
    endif
!     PUIS LES CHARGES A MAILLES ET/OU A NOEUDS TARDIFS
    do 10 k = 1, nchar
        nomcha = zk24(jchar+k-1)(1:8)
        call jeexin(nomcha(1:8)//'.TYPE', ier)
        if (ier .gt. 0) then
            call jeveuo(nomcha(1:8)//'.TYPE', 'L', jtypch)
            nomlig = nomcha(1:8)//'.CH'//zk8(jtypch) (1:2)// '.LIGRE.LIEL'
            call jeexin(nomlig, iret)
        else
            iret=0
        endif
        if (iret .gt. 0) then
            zk24(jlligr+nblig) = nomlig(1:19)
            nblig = nblig + 1
        endif
10  end do
!
    call jeecra(lligr, 'LONUTI', nblig)
!
    call numer2(nuposs, nblig, zk24(jlligr), ' ', solveu,&
                base, nu, nequag)

    call jedetr(lligr)
!
!
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.2', 'FIN', ' ')
    call jedema()
end subroutine
