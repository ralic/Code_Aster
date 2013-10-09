subroutine camoco(nomres, numref, intf, raid, raildl,&
                  inord)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  CALCUL DES MODES CONTRAINTS (DEPLACEMENT UNITAIRE IMPOSE)
!      ET STOCKAGE DANS LE CONCEPT MODE MECA A PARTIR D'UN
!                  NUMERO D'ORDRE
!-----------------------------------------------------------------------
!
! NOMRES    /I/: NOM DU CONCEPT RESULTAT
! NUMREF   /I/: NOM UT DU NUM_DDL DE REFERENCE
! INTF     /I/: NOM UT DE L'INTERF_DYNA EN AMONT
! RAID      /I/: NOM DE LA MATRICE RAIDEUR
! RAILDL    /M/: NOM DE LA MATRICE RAIDEUR FACTORISEE LDLT OU BLANC
! INORD     /M/: DERNIER NUMERO D'ORDRE UTILISE
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
#include "asterfort/wkvect.h"
!
!-----------------------------------------------------------------------
    integer :: i, iad, ier, ik, ino, inord
    integer ::  j, jj, lldeeq, lldes, llncmp, llnoin
    integer :: lltyp, ltddl, ltpar, nbcb, nbcmp, nbcont, nbcpmx
    integer :: nbdeb, nbec, nbfin, nbint, nbnoe, nbnot, neq
    integer :: ntail1, ntail2, numgd
!-----------------------------------------------------------------------
    parameter    (nbcpmx=300)
    character(len=6) :: pgc
    character(len=8) :: nomres, intf, typcou, nomnoe, nomcmp, mailla
    character(len=19) :: numref, numddl
    character(len=19) :: raildl, raid
    character(len=16) :: typdef
    character(len=24) :: desdef, deeq, temddl, tempar
    integer :: idec(nbcpmx)
!
!-----------------------------------------------------------------------
    data pgc /'CAMOCO'/
!-----------------------------------------------------------------------
!
!
    call jemarq()
    typdef='CONTRAINT'
!
!---------------------RECHERCHE DU NUMDDL ASSOCIE A LA MATRICE----------
!
    call dismoi('NOM_NUME_DDL', raid, 'MATR_ASSE', repk=numddl)
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
!
!------------REQUETTE ADRESSE DEFINITION INTERFACE ET TYPE--------------
!
    call jelira(intf//'.IDC_LINO', 'NMAXOC', nbint)
    call jeveuo(intf//'.IDC_TYPE', 'L', lltyp)
!
!-----------COMPTAGE DU NOMBRE DE NOEUDS CRAIG BAMPT--------------------
!
    nbdeb=nbnot
    nbfin=0
!
    do 10 j = 1, nbint
        call jelira(jexnum(intf//'.IDC_LINO', j), 'LONMAX', nbnoe)
        typcou=zk8(lltyp+j-1)
        if (typcou .eq. 'CRAIGB   ') then
            call jeveuo(jexnum(intf//'.IDC_LINO', j), 'L', llnoin)
            do 15 i = 1, nbnoe
                ik=zi(llnoin+i-1)
                nbfin=max(nbfin,ik)
                nbdeb=min(nbdeb,ik)
 15         continue
            call jelibe(jexnum(intf//'.IDC_LINO', j))
        endif
 10 end do
!
    call jelibe(intf//'.IDC_TYPE')
!
    if (nbfin .gt. 0) then
        nbcb=nbfin-nbdeb+1
    else
        nbcb=0
    endif
!
!
!----------ALLOCATION DU VECTEUR DES DDL A IMPOSER A 1------------------
!
    ntail1=(nbcb*nbcmp)*2
    ntail2=(nbcb*nbcmp)
!
!  TAILLE DOUBLE CAR PRESENCE EVENTUELLE DE DOUBLE LAGRANGE POUR LE
!   BLOCAGE
!
    if (ntail1 .eq. 0) goto 9999
    temddl='&&'//pgc//'.LISTE.DDL'
    tempar='&&'//pgc//'.PARA.NOCMP'
    call wkvect(temddl, 'V V I', ntail1, ltddl)
    call wkvect(tempar, 'V V K16', ntail2, ltpar)
    if (raildl .eq. '                  ') then
        raildl='&&'//pgc//'.RAID.LDLT'
        call facmtr(raid, raildl, ier)
    endif
!
!-------------COMPTAGE ET REPERAGE DES DEFORMEES A CALCULER-------------
!
    nbcont=0
!
    if (nbcb .gt. 0) then
        do 20 i = nbdeb, nbfin
!**************************************************************
!          ICOD=ZI(LLDES+2*NBNOT+I-1)
            call isdeco(zi(lldes+2*nbnot+(i-1)*nbec+1-1), idec, nbcmp)
!**************************************************************
            ino=zi(lldes+i-1)
            call jenuno(jexnum(mailla//'.NOMNOE', ino), nomnoe)
            do 30 j = 1, nbcmp
                if (idec(j) .eq. 1) then
                    jj=-j
                    nbcont=nbcont+1
                    nomcmp=zk8(llncmp+j-1)
                    zk16(ltpar+nbcont-1)=nomnoe//nomcmp
                    iad=ltddl+(nbcont-1)*2
                    call cheddl(zi(lldeeq), neq, ino, jj, zi(iad),&
                                2)
                endif
 30         continue
 20     continue
    endif
!
!
!
!------------------CALCUL DES MODES CONTRAINTS--------------------------
!
    call defsta(nomres, numref, raildl, zi(ltddl), zk16(ltpar),&
                2, nbcont, typdef, inord)
!
!----------------------MENAGE-------------------------------------------
!
    call jedetr(temddl)
    call jedetr(tempar)
    call jelibe(deeq)
!
!
9999 continue
    call jedema()
end subroutine
