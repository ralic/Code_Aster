subroutine flexib(basmod, nbmod, flex, nl, nc,&
                  numl, numc)
! aslint: disable=W1306
    implicit none
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
!***********************************************************************
!  P. RICHARD     DATE 09/04/91
!-----------------------------------------------------------------------
!  BUT : CALCULER LA MATRICE DE FLEXIBILITE RESIDUELLE ASSOCIEE
!        A UN PROBLEME CYCLIQUE AVEC INTERFACE MAC NEAL OU AUCUN
!        (FLEXIBILITE NULLE DANS LE CAS AUCUN)
!
!        SEULE LA SOUS MATRICE RELATIVE AUX DEFORMEES (COLONNES) D'UNE
!        INTERFACE ET AUX DDL D'UNE AUTRE (LIGNES) EST CALCULEE
!
!        POUR LES LIGNES IL EST POSSIBLE DE NE PAS DONNER UNE INTERFACE
!        MAIS DE PRENDRE TOUTES LES LIGNES ( = TOUS LES DDL PHYSIQUES)
!        IL SUFFIT POUR CELA DE DONNER UN NUMERO D'INTERFACE NUML = 0
!-----------------------------------------------------------------------
!
! BASMOD /I/ : NOM UTILISATEUR DE LA BASE MODALE
! NBMOD  /I/ : NOMBRE DE MODES PROPRES UTILISES
! FLEX   /O/ : MATRICE DE FLEXIBILITE RESIDUELLE
! NL     /I/ : NOMBRE DE LIGNES DE LA MATRICE DE FLEXIBILITE
! NC     /I/ : NOMBRE DE COLONNES DE LA MATRICE DE FLEXIBILITE
! NUML   /I/ : NUMERO DE L'INTERFACE DE DDL RELATIFS AUX LIGNES
! NUMC   /I/ : NUMERO DE L'INTERFACE DE DDL RELATIFS AUX COLONNES
!
!
!
!
!
#include "jeveux.h"
!
#include "asterfort/bmnodi.h"
#include "asterfort/bmradi.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
!
    integer           :: nl, nc
    real(kind=8)      :: flex(nl, nc)
    character(len=6)  :: pgc
    character(len=8)  :: basmod, typint, intf, kbid, k8bid
    character(len=19) :: numddl
    character(len=24) :: chamva, noeint
    character(len=24) :: valk
!
!----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, iddeeq, iord, iran, iret, j
    integer :: jj, k, kk, ldkge, ldmge, llcham, lldes
    integer :: llnoc,llnol,lltyp,ltextc,ltextl,ltorc
    integer :: ltvec, nbmod, nbnoc, nbnol, nbnot, neq
    integer :: numc, numl
    real(kind=8) :: toto, xkgen, xx
!-----------------------------------------------------------------------
    data pgc /'FLEXIB'/
!-----------------------------------------------------------------------
!
    call jemarq()
    do 10 i = 1, nl
        do 10 j = 1, nc
            flex(i,j)=0.d0
10      continue
!
! --- RECUPERATION CONCEPTS AMONT
!
    call dismoi('F', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid, intf, iret)
    call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid, numddl, iret)

    if (intf .eq. '        ') then
        valk = basmod
        call u2mesg('F', 'ALGORITH13_17', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
! --- TEST SUR LE TYPE D'INTERFACE
!
    call jeveuo(intf//'.IDC_TYPE', 'L', lltyp)
    typint=zk8(lltyp+numc-1)
    call jelibe(intf//'.IDC_TYPE')
    if (typint .eq. 'AUCUN   ') goto 9999
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL
!
    call wkvect('&&'//pgc//'.ORDREC', 'V V I', nc, ltorc)
    call wkvect('&&'//pgc//'.EXTRACC', 'V V I', nc, ltextc)
    call wkvect('&&'//pgc//'.EXTRACL', 'V V I', nl, ltextl)
!
! --- RECUPERATION DU NOMBRE DE DDL PHYSIQUES ASSEMBLES
!
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8bid, iret)
!----ON AJOUT .NUME POUR OBTENIR LE PROF_CHNO
    numddl(15:19)='.NUME'
    call jeveuo(numddl//'.DEEQ', 'L', iddeeq)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS DES INTERFACES
!
    noeint=intf//'.IDC_LINO'
!
    if (numl .gt. 0) then
        call jelira(jexnum(noeint, numl), 'LONMAX', nbnol)
        call jeveuo(jexnum(noeint, numl), 'L', llnol)
    else
        nbnol=0
    endif
!
    call jelira(jexnum(noeint, numc), 'LONMAX', nbnoc)
    call jeveuo(jexnum(noeint, numc), 'L', llnoc)
!
! --- RECUPERATION DU DESCRIPTEUR DES DEFORMEES
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldes)
    call jelira(intf//'.IDC_DEFO', 'LONMAX', nbnot)
    nbnot=nbnot/3
!
! --- RECUPERATION DES NUMEROS D'ORDRE DES DEFORMEES (COLONNES)
!     ET RANGS DES DDL D'INTERFACE (LIGNES) DANS VECTEUR ASSEMBLE
!
! --- RECUPERATION NUMERO ORDRE DEFORMEES ET RANG DDL POUR COLONNES
!
    kbid=' '
    call bmnodi(basmod, kbid, '        ', numc, nc,&
                zi(ltorc), ibid)
    call bmradi(basmod, kbid, '        ', numc, nc,&
                zi(ltextc), ibid)
!
! --- RECUPERATION DDL PHYSIQUES POUR LES LIGNES
!
    if (numl .gt. 0) then
        call bmradi(basmod, kbid, '        ', numl, nl,&
                    zi(ltextl), ibid)
    else
        do 45 i = 1, neq
            zi(ltextl+i-1)=i
45      continue
    endif
!
    if (numl .gt. 0) then
        call jelibe(jexnum(noeint, numl))
    endif
    call jelibe(jexnum(noeint, numc))
    call jelibe(intf//'.IDC_DEFO')
!
! --- EXTRACTION PARTIE INTERFACE DE FLEXIBILITE
!
    do 60 i = 1, nc
        call wkvect('&&'//pgc//'.VECT', 'V V R', neq, ltvec)
        iord=zi(ltorc+i-1)
        call dcapno(basmod, 'DEPL    ', iord, chamva)
        call jeveuo(chamva, 'L', llcham)
        call dcopy(neq, zr(llcham), 1, zr(ltvec), 1)
        call zerlag(neq, zi(iddeeq), vectr=zr(ltvec))
!
        do 70 j = 1, nl
!
!  EXTRACTION DDL
!
            iran=zi(ltextl+j-1)
            xx=zr(ltvec+iran-1)
            flex(j,i)=xx
70      continue
        call jelibe(chamva)
        call jedetr('&&'//pgc//'.VECT')
60  continue
!
! --- SUPPRESSION CONTRIBUTION STATIQUE DES MODES CONNUS
!
    do 80 i = 1, nbmod
!
        call rsadpa(basmod, 'L', 1, 'RIGI_GENE', i,&
                    0, ldkge, k8bid)
        xkgen=zr(ldkge)
        call rsadpa(basmod, 'L', 1, 'MASS_GENE', i,&
                    0, ldmge, k8bid)
!
        call dcapno(basmod, 'DEPL    ', i, chamva)
        call jeveuo(chamva, 'L', llcham)
        call wkvect('&&'//pgc//'.VECT', 'V V R', neq, ltvec)
        call dcopy(neq, zr(llcham), 1, zr(ltvec), 1)
        call zerlag(neq, zi(iddeeq), vectr=zr(ltvec))
!
        do 90 j = 1, nc
            do 95 k = 1, nl
                jj=zi(ltextc+j-1)
                kk=zi(ltextl+k-1)
                toto=zr(ltvec+jj-1)*zr(ltvec+kk-1)/xkgen
                flex(k,j)=flex(k,j)-toto
95          continue
90      continue
        call jelibe(chamva)
        call jedetr('&&'//pgc//'.VECT')
80  continue
!
    call jedetr('&&'//pgc//'.ORDREC')
    call jedetr('&&'//pgc//'.EXTRACC')
    call jedetr('&&'//pgc//'.EXTRACL')
!
9999  continue
    call jedema()
end subroutine
