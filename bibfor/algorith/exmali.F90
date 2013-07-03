subroutine exmali(basmod, nomint, numint, nommat, base,&
                  nblig, nbcol, ord, ii)
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
!***********************************************************************
!    P. RICHARD     DATE 23/05/91
!-----------------------------------------------------------------------
!  BUT:  < EXTRACTION DES DDL RELATIFS A UNE INTERFACE >
    implicit none
!
!  CONSISTE A EXTRAIRE LES VALEURS DANS LA BASE MODALE DES DDL ACTIFS
!   D'UNE INTERFACE
!
!-----------------------------------------------------------------------
!
! BASMOD   /I/: NOM UT DE LA BASE_MODALE
! NOMINT   /I/: NOM DE L'INTERFACE
! NUMINT   /I/: NUMERO DE L'INTERFACE
! NOMMAT   /I/: NOM K24 DE LA MATRICE RESULTAT A ALLOUE
! BASE     /I/: NOM K1 DE LA BASE POUR CREATION NOMMAT
! NBLIG    /I/: NOMBRE DE LIGNES DE LA MATRICE LIAISON CREE
! NBCOL    /I/: NOMBRE DE COLONNES DE LA MATRICE LIAISON CREE
!
!
!
!
!
#include "jeveux.h"
!
#include "asterfort/bmrdda.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=6) :: pgc
    character(len=8) :: basmod, nomint, lintf, kbid
    character(len=24) :: chamva, nommat
    integer :: ord, ii, llref, numint, nbdef, nbcol, ibid, nbddl, nblig, ltrang
    integer :: llcham, iran, iad, i, j, ldmat, ier
!
!-----------------------------------------------------------------------
    data pgc /'EXMALI'/
!-----------------------------------------------------------------------
!
!---------------------RECUPERATION LISTE_INTERFACE AMONT----------------
!
    call jemarq()
    call jeveuo(basmod//'           .REFD', 'L', llref)
    lintf=zk24(llref+4)
!
!----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
!
    if (nomint .ne. '             ') then
        call jenonu(jexnom(lintf//'.IDC_NOMS', nomint), numint)
    endif
!
!
!----------------RECUPERATION DU NOMBRE DE DDL GENERALISES--------------
!
    call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbdef,&
                kbid, ier)
    nbcol=nbdef
!
!----RECUPERATION DU NOMBRE DE DDL  ET NOEUDS ASSOCIES A L'INTERFACE----
!
    kbid=' '
    call bmrdda(basmod, kbid, nomint, numint, 0,&
                ibid, nbddl, ord, ii)
    nblig=nbddl
!
!----------------ALLOCATION DU VECTEUR DES RANGS DES DDL----------------
!
    call wkvect('&&'//pgc//'.RAN.DDL', 'V V I', nbddl, ltrang)
!
!-------------DETERMINATION DES RANG DES DDL ASSOCIES A INTERFACE-------
!
    kbid=' '
    call bmrdda(basmod, kbid, nomint, numint, nbddl,&
                zi(ltrang), ibid, ord, ii)
!
!-----------------ALLOCATION MATRICE LIAISON RESULTAT-------------------
!
!
    call wkvect(nommat, base//' V R', nblig*nbcol, ldmat)
!
!-------------------------EXTRACTION------------------------------------
!
    do 10 i = 1, nbdef
!
        call dcapno(basmod, 'DEPL    ', i, chamva)
        call jeveuo(chamva, 'L', llcham)
!
        do 20 j = 1, nbddl
            iran=zi(ltrang+j-1)
            iad=ldmat+((i-1)*nbddl)+j-1
            zr(iad)=zr(llcham+iran-1)
20      continue
!
!
10  end do
!
!
    call jedetr('&&'//pgc//'.RAN.DDL')
!
    call jedema()
end subroutine
