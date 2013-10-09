subroutine genugl(profno, indirf, modgen, mailsk)
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
!  P. RICHARD     DATE 16/11/92
!-----------------------------------------------------------------------
!  BUT: < GENERALE NUMEROTATION GLOBALE >
!
!  CREER, A PARTIR D'UN MODELE GENERALISE ET D'UN MAILLAGE GLOBAL
!  SQUELETTE, LE PROFIL CHAMNO ET UNE FAMILLE NUMEROTEE DONT CHAQUE
!  OBJET CORRESPOND A UNE SOUS-STRUCTURE, ET EST DIMENSIONNE A 2*NBDDL
!  ENGENDRE PAR LES SOUS-STRUCTURES :
!            2*(I-1)+1 --> NUMERO EQUATION DANS NUMDDL SST
!            2*(I-1)+2 --> NUMERO EQUATION DANS PROFNO GLOBAL
!
!-----------------------------------------------------------------------
!
! PROFNO  /I/ : NOM K19 DU PROF_CHNO A CREER
! INDIRF  /I/ : NOM K24 DE LA FAMILLE DES INDIRECTIONS A CREER
! MODGEN  /I/ : NOM DU MODELE GENERALISE EN AMONT
! MAILSK  /I/ : NOM DU MAILLAGE SKELETTE
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
!
!-----------------------------------------------------------------------
    integer :: i, ibid, icomp, iddn, idds, iec, ieq
    integer :: ipoint, j, k, lddeeq, ldinse, ldnueq
    integer :: ldprno, linueq, llinsk, llnueq, llprno, lttds, nbcmp
    integer :: nbcou, nbcpmx, nbddl, nbec, nbnot, nbsst, nddlt
    integer :: ntail, numno, nusst
!-----------------------------------------------------------------------
    parameter    (nbcpmx=300)
    character(len=6) :: pgc
    character(len=8) :: mailsk, modgen, kbid
    character(len=8) :: k8bid
    character(len=19) :: numddl, profno
    character(len=24) :: indirf, lili, prno, deeq, nueq
    integer :: idec(nbcpmx)
!
!-----------------------------------------------------------------------
    data pgc /'GENUGL'/
!-----------------------------------------------------------------------
!
    call jemarq()
    kbid='  '
    call mgutdm(modgen, kbid, 1, 'NB_CMP_MAX', nbcmp,&
                k8bid)
!
!-----RECUPERATION DU NOMBRE D'ENTIERS CODES DE LA GRANDEUR DEPL_R------
!
    call dismoi('NB_EC', 'DEPL_R', 'GRANDEUR', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
!-----RECUPERATION DU NOMBRE DE SOUS-STRUCTURES-------------------------
!
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
!
!-----RECUPERATION DIMENSION MAILLAGE SQUELETTE-------------------------
!
    call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
!-----RECUPERATION DU .INV.SKELETON-------------------------------------
!
    call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
!
!-----ALLOCATION DU VECTEUR DE TRAVAIL POUR STOCKAGE NOMBRE DE DDL
!     GLOBAUX ENGENDRE PAR SOUS STRUCTURE
!
    call wkvect('&&'//pgc//'.TAIL.DDL.SST', 'V V I', nbsst, lttds)
!
!-----BOUCLE DE COMPTAGE DES DDL FINAUX---------------------------------
!
    nddlt=0
    do i = 1, nbsst
        kbid='  '
        call mgutdm(modgen, kbid, i, 'NOM_NUME_DDL', ibid,&
                    numddl)
        numddl(15:19)='.NUME'
        call jenonu(jexnom(numddl//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(numddl//'.PRNO', ibid), 'L', llprno)
        do j = 1, nbnot
            nusst=zi(llinsk+j-1)
            numno=zi(llinsk+nbnot+j-1)
            if (nusst .eq. i) then
                nddlt=nddlt+zi(llprno+(numno-1)*(2+nbec)+1)
                zi(lttds+i-1)=zi(lttds+i-1)+zi(llprno+(numno-1)*(2+&
                nbec)+1)
            endif
        end do
    end do
!
!-----ALLOCATION DES DIVERS OBJETS-------------------------------------
!
    lili=profno//'.LILI'
    prno=profno//'.PRNO'
    deeq=profno//'.DEEQ'
    nueq=profno//'.NUEQ'
!
    call jecreo(lili, 'G N K24')
    call jeecra(lili, 'NOMMAX', 2)
    call wkvect(deeq, 'G V I', nddlt*2, lddeeq)
    call wkvect(nueq, 'G V I', nddlt, ldnueq)
    call jecrec(prno, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                2)
    call jecroc(jexnom(prno(1:19)//'.LILI', '&MAILLA'))
    call jecroc(jexnom(prno(1:19)//'.LILI', 'LIAISONS'))
    call jecrec(indirf, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbsst)
!
    do i = 1, nbsst
        ntail=2*zi(lttds+i-1)
        if (ntail .gt. 0) then
            call jeecra(jexnum(indirf, i), 'LONMAX', ntail)
            call jecroc(jexnum(indirf, i))
        endif
    end do
!
!-----REMPLISSAGE DES OBJETS EVIDENTS-----------------------------------
!
    call jeecra(jexnum(prno, 1), 'LONMAX', nbnot*(2+nbec))
    call jeecra(jexnum(prno, 2), 'LONMAX', 1)
    call jeecra(prno, 'LONT', nbnot*(2+nbec)+1)
!
!-----REMPLISSAGE DES OBJETS--------------------------------------------
!
    call jenonu(jexnom(prno(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(prno, ibid), 'E', ldprno)
!
    icomp=0
!
!  BOUCLE SUR LES SST DU MODELE GENERALISE
!
    do i = 1, nbsst
        nbcou=zi(lttds+i-1)
        idds=0
!
!  TEST SI LA SST COURANTE ENGENDRE DES DDL GLOBAUX
!
        if (nbcou .gt. 0) then
            kbid='  '
            call mgutdm(modgen, kbid, i, 'NOM_NUME_DDL', ibid,&
                        numddl)
            numddl(15:19)='.NUME'
            call jenonu(jexnom(numddl//'.LILI', '&MAILLA'), ibid)
            call jeveuo(jexnum(numddl//'.PRNO', ibid), 'L', llprno)
            call jeveuo(numddl//'.NUEQ', 'L', llnueq)
            call jeveuo(jexnum(indirf, i), 'E', ldinse)
!
!  BOUCLE SUR LES DDL GLOBAUX
!
            do j = 1, nbnot
                nusst=zi(llinsk+j-1)
!
!  TEST SI LE NOEUD GLOBAL EST ENGENDRE PAR LA SST
!
                if (nusst .eq. i) then
                    numno=zi(llinsk+nbnot+j-1)
                    ieq=zi(llprno+(numno-1)*(2+nbec))
                    nbddl=zi(llprno+(numno-1)*(2+nbec)+1)
                    call isdeco(zi(llprno+(numno-1)*(2+nbec)+2), idec, nbcmp)
                    zi(ldprno+(j-1)*(2+nbec))=icomp+1
                    zi(ldprno+(j-1)*(2+nbec)+1)=nbddl
                    do iec = 1, nbec
                        zi(ldprno+(j-1)*(2+nbec)+1+iec)= zi(llprno+(&
                        numno-1)*(2+nbec)+1+iec)
                    end do
                    iddn=0
                    do k = 1, nbcmp
                        if (idec(k) .gt. 0) then
                            iddn=iddn+1
                            icomp=icomp+1
                            zi(lddeeq+(icomp-1)*2)=j
                            zi(lddeeq+(icomp-1)*2+1)=k
                            zi(ldnueq+icomp-1)=icomp
                            linueq=zi(llnueq+ieq+iddn-2)
                            ipoint=ldinse+2*idds-1
                            zi(ipoint+1)=linueq
                            zi(ipoint+2)=icomp
                            idds=idds+1
                        endif
                    end do
                endif
            end do
            call jelibe(numddl//'.NUEQ')
            call jelibe(jexnum(indirf, i))
        endif
    end do
!
!-----SAUVEGARDE DES OBJETS---------------------------------------------
!
    call jedetr('&&'//pgc//'.TAIL.DDL.SST')
!
    call jedema()
end subroutine
