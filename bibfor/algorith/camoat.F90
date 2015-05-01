subroutine camoat(nomres, numref, intf, raid, raildl,&
                  inord)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  CALCUL DES MODES D'ATTACHE (FORCE UNITAIRE IMPOSE)
!       A PARTIR D'UN CONCEPT INTERF_DYNA'
!      ET STOCKAGE DANS LE CONCEPT BASE_MODALE A PARTIR D'UN
!                  NUMERO D'ORDRE
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM DU CONCEPT RESULTAT
! NUMREF   /I/: NOM UT DU NUM_DDL DE REFERENCE
! INTF     /I/: NOM UT DE L'INTERF_DYNA EN AMONT
! RAID     /I/: NOM DE LA MATRICE RAIDEUR
! RAILDL   /M/: NOM DE LA MATRICE RAIDEUR FACTORISEE LDLT OU BLANC
! INORD    /M/: DERNIER NUMERO D'ORDRE UTILISE
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/cheddl.h"
#include "asterfort/defsta.h"
#include "asterfort/dismoi.h"
#include "asterfort/facmtr.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!-----------------------------------------------------------------------
    integer :: i, ier, ik, ino, inord
    integer :: j, lldeeq, lldes, llncmp, llnoin,  ltddl
    integer :: ltpar, nbatta, nbcmp, nbcpmx, nbdeb, nbec, nbfin
    integer :: nbint, nbmn, nbnoe, nbnot, neq, ntail, numgd
!
!-----------------------------------------------------------------------
    parameter (nbcpmx=300)
    character(len=6) :: pgc
    character(len=8) :: nomres, intf, typcou, nomnoe, nomcmp, mailla
    character(len=19) :: numddl, numref
    character(len=19) :: raildl, raid
    character(len=16) :: typdef
    character(len=24) :: desdef, deeq, temddl, tempar
    integer :: idec(nbcpmx)
    character(len=8), pointer :: idc_type(:) => null()
!
!-----------------------------------------------------------------------
    data pgc /'CAMOAT'/
!-----------------------------------------------------------------------
!
    call jemarq()
    typdef='ATTACHE'
!
!---------------------RECHERCHE DU NUMDDL ASSOCIE A LA MATRICE----------
!
    call dismoi('NOM_NUME_DDL', raid, 'MATR_ASSE', repk=numddl)
    numddl(15:19)='.NUME'
!
!---------------------REQUETTE DU DEEQ DU NUMDDL------------------------
!
    numddl(15:19)='.NUME'
    deeq=numddl//'.DEEQ'
    call jeveuo(deeq, 'L', lldeeq)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
!
!--------------------RECUPERATION DU MAILLAGE---------------------------
!
    call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
!
!----RECUPERATION DES DONNEES RELATIVES A LA GRANDEUR SOUS-JACENTE------
!
    call dismoi('NB_CMP_MAX', intf, 'INTERF_DYNA', repi=nbcmp)
    call dismoi('NB_EC', intf, 'INTERF_DYNA', repi=nbec)
    call dismoi('NUM_GD', intf, 'INTERF_DYNA', repi=numgd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', llncmp)
!
!-----------REQUETTE ADRESSE DE LA TABLE DESCRIPTION DES DEFORMEES------
!
    desdef=intf//'.IDC_DEFO'
    call jeveuo(desdef, 'L', lldes)
    call jelira(desdef, 'LONMAX', nbnot)
!**************************************************************
    nbnot = nbnot/(2+nbec)
!      NBNOT=NBNOT/3
!**************************************************************
!
!------------REQUETTE ADRESSE DEFINITION INTERFACE ET TYPE--------------
!
    call jelira(intf//'.IDC_LINO', 'NMAXOC', nbint)
    call jeveuo(intf//'.IDC_TYPE', 'L', vk8=idc_type)
!
!-----------COMPTAGE DU NOMBRE DE NOEUDS MAC NEAL-----------------------
!
    nbdeb=nbnot
    nbfin=0
!
    do j = 1, nbint
        call jelira(jexnum(intf//'.IDC_LINO', j), 'LONMAX', nbnoe)
        typcou=idc_type(j)
        if (typcou .eq. 'MNEAL   ') then
            call jeveuo(jexnum(intf//'.IDC_LINO', j), 'L', llnoin)
            do i = 1, nbnoe
                ik=zi(llnoin+i-1)
                nbfin=max(nbfin,ik)
                nbdeb=min(nbdeb,ik)
            end do
            call jelibe(jexnum(intf//'.IDC_LINO', j))
        endif
    end do
!
    call jelibe(intf//'.IDC_TYPE')
!
    if (nbfin .gt. 0) then
        nbmn=nbfin-nbdeb+1
    else
        nbmn=0
    endif
!
!
!----------ALLOCATION DU VECTEUR DES DDL A IMPOSER A 1------------------
!                    ET DES VALEURS DES PARAMETRES NOEUD_CMP
    ntail=nbmn*nbcmp
    if (ntail .eq. 0) goto 999
!
    temddl='&&'//pgc//'.LISTE.DDL'
    tempar='&&'//pgc//'.PARA.NOCMP'
    call wkvect(temddl, 'V V I', ntail, ltddl)
    call wkvect(tempar, 'V V K16', ntail, ltpar)
    if (raildl .eq. '                   ') then
        raildl='&&'//pgc//'.RAID.LDLT'
        call facmtr(raid, raildl, ier)
        if (ier .eq. -2) then
            call utmess('F', 'ALGORITH12_40')
        endif
    endif
!
!
!-------------COMPTAGE ET REPERAGE DES DEFORMEES A CALCULER-------------
!
!
    nbatta=0
!
    if (nbmn .gt. 0) then
        do i = nbdeb, nbfin
!**************************************************************
!          ICOD=ZI(LLDES+2*NBNOT+I-1)
            call isdeco(zi(lldes+2*nbnot+(i-1)*nbec+1-1), idec, nbcmp)
!**************************************************************
            ino=zi(lldes+i-1)
            call jenuno(jexnum(mailla//'.NOMNOE', ino), nomnoe)
            do j = 1, nbcmp
                if (idec(j) .eq. 1) then
                    nbatta=nbatta+1
                    nomcmp=zk8(llncmp+j-1)
                    zk16(ltpar+nbatta-1)=nomnoe//nomcmp
                    call cheddl(zi(lldeeq), neq, ino, j, zi(ltddl+nbatta- 1),&
                                1)
                endif
            end do
        end do
    endif
!
!
!------------------CALCUL DES MODES D'ATTACHE---------------------------
!
!
    call defsta(nomres, numref, raildl, zi(ltddl), zk16(ltpar),&
                1, nbatta, typdef, inord)
!
!
!-------------------MENAGE----------------------------------------------
!
    call jedetr(temddl)
    call jedetr(tempar)
    call jelibe(deeq)
!
!
999 continue
    call jedema()
end subroutine
