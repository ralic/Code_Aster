subroutine cynugl(profno, indirf, modcyc, mailsk)
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
!    P. RICHARD     DATE 10/02/92
!-----------------------------------------------------------------------
!  BUT: < CYCLIQUE NUMEROTATION GLOBALE >
!
!    CREER A PARTIR D'UN RESULTAT CYCLIQUE ET UN MAILLAGE GLOBAL
!  SQUELLETTE LE PROFIL CHAMNO ET UNE FAMILLE NUMEROTEE
! ,DONT CHAQUE OBJET CORRESPOND A UN SECTEUR, ET EST DIMENSIONNE
!  A 2*NBDDL ENGENDRE PAR LE SECTEUR:C
!
!            2*(I-1)+1 --> NUMERO EQUATION DANS NUMDDL SECTEUR
!            2*(I-1)+2 --> NUMERO EQUATION DANS PROFNO GLOBAL
!
!-----------------------------------------------------------------------
!
! NOM----- / /:
!
! PROFNO   /I/: NOM K19 DU PROF_CHNO A CREER
! INDIRF   /I/: NOM K24 DE LA FAMILLE DES INDIRECTIONS A CREER
! MODCYC   /I/: NOM DU RESULTAT CYCLIQUE EN AMONT
! MAILSK   /I/: NOM DU MAILLAGE SKELETTE
!
!
!
!
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
!
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, icomp, iec, ieq, ipoint
    integer :: iret, j, lddeeq, ldnueq, ldprno, linueq, llinsk
    integer :: llnoms, llnueq, llprno, llref1, llref2, ltinse, lttds
    integer :: nbcmp, nbcpmx, nbddl, nbec, nbnot, nbsec, nddlt
    integer :: neqsec, nsecpr, ntail, numnos, numsec
!-----------------------------------------------------------------------
    parameter    (nbcpmx=300)
    character(len=6) :: pgc
    character(len=8) :: modcyc, mailsk, basmod, intf
    character(len=8) :: k8bid
    character(len=19) :: numddl, profno
    character(len=24) :: indirf, lili, prno, deeq, nueq
    integer :: idec(nbcpmx)
!
!-----------------------------------------------------------------------
!
    call jemarq()
    pgc='CYNUGL'
!
!
!-------------------RECUPERATION DE LA BASE MODALE----------------------
!
    call jeveuo(modcyc//'.CYCL_REFE', 'L', llref1)
    basmod=zk24(llref1+2)
    call jelibe(modcyc//'.CYCL_REFE')
!
!----------------RECUPERATION DU NUMDDLET DE L'INTERF_DYNA--------------
!
!
    call jeveuo(basmod//'           .REFD', 'L', llref2)
    intf  =zk24(llref2+4)
    numddl=zk24(llref2+3)
    call jelibe(basmod//'           .REFD')
!
!---------------RECUPERATION DU NOMBRE DE COMPOSANTES-------------------
!
    call dismoi('F', 'NB_CMP_MAX', intf, 'INTERF_DYNA', nbcmp,&
                k8bid, iret)
    call dismoi('F', 'NB_EC', intf, 'INTERF_DYNA', nbec,&
                k8bid, iret)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
!---------------RECUPERATION DU NOMBRE DE SECTEURS----------------------
!
!
    call jeveuo(modcyc//'.CYCL_NBSC', 'L', llnoms)
    nbsec=zi(llnoms)
    call jelibe(modcyc//'.CYCL_NBSC')
!
!-------------RECUPERATION DIMENSION MAILLAGE SQUELETTE-----------------
!
    call dismoi('F', 'NB_NO_MAILLA', mailsk, 'MAILLAGE', nbnot,&
                k8bid, iret)
!
!------------RECUPERATION DU .INV.SKELETON------------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
!
!--------------RECUPERATION DU PRNO DU SECTEUR--------------------------
!
!----ON AJOUT .NUME POUR OBTENIR LE PROF_CHNO
    numddl(15:19)='.NUME'
    call jenonu(jexnom(numddl//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(numddl//'.PRNO', ibid), 'L', llprno)
    call jeveuo(numddl//'.NUEQ', 'L', llnueq)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neqsec,&
                k8bid, iret)
!
!--------------------ALLOCATION DU VECTEUR DE TRAVAIL-------------------
!     POUR STOCKAGE NOMBRE DE DDL GLOBAUX ENGENDRE PAR SECTEUR
!
    call wkvect('&&'//pgc//'.TAIL.DDL.SECT', 'V V IS', nbsec, lttds)
!
!--------------BOUCLE DE COMPTAGE DES DDL FINAUX------------------------
!
    nddlt=0
    do 10 i = 1, nbnot
        numsec=zi(llinsk+i-1)
        numnos=zi(llinsk+nbnot+i-1)
        nddlt=nddlt+zi(llprno+(numnos-1)*(2+nbec)+1)
        zi(lttds+numsec-1)=zi(lttds+numsec-1)+ zi(llprno+(numnos-1)*(&
        2+nbec)+1)
10  end do
!
!-----------------ALLOCATION DES DIVERS OBJETS--------------------------
!
    lili=profno//'.LILI'
    prno=profno//'.PRNO'
    deeq=profno//'.DEEQ'
    nueq=profno//'.NUEQ'
!
    call jecreo(lili, 'G N K24')
    call jeecra(lili, 'NOMMAX', 2)
!
!
    call wkvect(deeq, 'G V I', nddlt*2, lddeeq)
!
!
    call wkvect(nueq, 'G V I', nddlt, ldnueq)
!
    call jecrec(prno, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                2)
!
!
    call jecroc(jexnom(prno(1:19)//'.LILI', '&MAILLA'))
    call jecroc(jexnom(prno(1:19)//'.LILI', 'LIAISONS'))
!
!
    call jecrec(indirf, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbsec)
!
!
    do 20 i = 1, nbsec
        call jecroc(jexnum(indirf, i))
        ntail=2*zi(lttds+i-1)
        call jeecra(jexnum(indirf, i), 'LONMAX', ntail)
        zi(lttds+i-1)=0
20  end do
!
!
!---------------REMPLISSAGE DES OBJETS EVIDENTS-------------------------
!
    call jeecra(jexnum(prno, 1), 'LONMAX', nbnot*(2+nbec))
    call jeecra(jexnum(prno, 2), 'LONMAX', 1)
    call jeecra(prno, 'LONT', nbnot*(2+nbec)+1)
!
!-------------------------REMPLISSAGE DES OBJETS------------------------
!  SUPPOSE QUE LES NOEUDS GLOBAUX SONT RANGES PAR SOUS-STRUCTURES
!
!
    call jenonu(jexnom(prno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(prno, ibid), 'E', ldprno)
!
    nsecpr=1
    call jeveuo(jexnum(indirf, nsecpr), 'E', ltinse)
    icomp=0
    do 30 i = 1, nbnot
!
        numsec=zi(llinsk+i-1)
        numnos=zi(llinsk+nbnot+i-1)
        ieq=zi(llprno+(numnos-1)*(2+nbec))
        nbddl=zi(llprno+(numnos-1)*(2+nbec)+1)
        call isdeco(zi(llprno+(numnos-1)*(2+nbec)+2), idec, nbcmp)
!
        zi(ldprno+(i-1)*(2+nbec))=icomp+1
        zi(ldprno+(i-1)*(2+nbec)+1)=nbddl
        do 40 iec = 1, nbec
            zi(ldprno+(i-1)*(2+nbec)+1+iec)= zi(llprno+(numnos-1)*(2+&
            nbec)+1+iec)
40      continue
        if (numsec .ne. nsecpr) then
            call jelibe(jexnum(indirf, nsecpr))
            nsecpr=numsec
            call jeveuo(jexnum(indirf, nsecpr), 'E', ltinse)
        endif
        iad=0
        do 50 j = 1, nbcmp
            if (idec(j) .gt. 0) then
                iad=iad+1
                icomp=icomp+1
                zi(lddeeq+(icomp-1)*2)=i
                zi(lddeeq+(icomp-1)*2+1)=j
                zi(ldnueq+icomp-1)=icomp
                linueq=zi(llnueq+ieq+iad-2)
                ipoint=ltinse-1+2*zi(lttds+numsec-1)
                zi(ipoint+1)=linueq
                zi(ipoint+2)=icomp
                zi(lttds+numsec-1)=zi(lttds+numsec-1)+1
            endif
50      continue
30  end do
!
    call jelibe(jexnum(indirf, nsecpr))
!
!------------------SAUVEGARDE DES OBJETS--------------------------------
!
    call jedetr('&&'//pgc//'.TAIL.DDL.SECT')
!
    call jedema()
end subroutine
