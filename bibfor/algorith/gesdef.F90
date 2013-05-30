subroutine gesdef(nomres, numddl)
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
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT: < GESTION DES DEFORMEES A CALCULER >
    implicit none
!
!     EN FONCTION DES DDL ACTIFS
!  ET DES MASQUE DE DDL AUX NOEUDS
!   FINIR DE REMPLIR LA TABLEAU DE DESCRIPTION DES DEFORMEES
!
! UTILISATION D'UN TABLEAU VOLATIL DE DESCRIPTION DES DDL PORTES PAR
!   LES NOEUDS:
!
!  ATTENTION: RESTRICTION A 12 DDL PAR NOEUD 6 PHYSIQUES ET LES 6
!            LAGRANGES CORRESPONDANTS
!
!    DX=1    (1)    LAG SUR UN DX=-1   (7)        () ASTER
!    DY=2    (2)    LAG SUR UN DY=-2   (7)
!    DZ=3    (3)    LAG SUR UN DZ=-3   (7)
!    DRX=4    (4)    LAG SUR UN DRX=-4   (7)
!    DRY=5    (5)    LAG SUR UN DRY=-5   (7)
!    DRZ=6    (6)    LAG SUR UN DRZ=-6   (7)
!
!   REMARQUE:LES DDL DU IPRNO NE PORTE QUE DE DDL A CODE POSITIF CAR LE
!     IPRNO CORRESPOND AU PRNO DU LIGREL MAILLAGE -> DECODAGE SUR 6
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DES RESULTATS DE L'OPERATEUR
! NUMDDL   /I/: NOM DE LA NUMEROTATION CORRESPONDANT AU PB
! NBDEF   /O/: NOMBRE TOTAL DE DEFORMEES A CALCULER
!
!
!
!
    include 'jeveux.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/maskau.h'
    include 'asterfort/maskcb.h'
    include 'asterfort/maskmn.h'
    include 'asterfort/recddl.h'
    include 'asterfort/wkvect.h'
    character(len=6) :: pgc
    character(len=8) :: nomres
    character(len=19) :: numddl
    character(len=24) :: desdef, deeq, temmat, temidc
    integer :: ikyp(4)
    character(len=8) :: k8bid
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad1, iad2, ik, iret, lddesc, lldeeq
    integer :: lldes, ltidec, ltmat, nbcmp, nbdef, nbec, nbno
    integer :: nbnot, nbtem, neq, nomax
!-----------------------------------------------------------------------
    data pgc/'GESDEF'/
!-----------------------------------------------------------------------
!
!------------RECUPERATION DONNEES GRANDEURS SOUS-JACENTE----------------
!             ET CREATION VECEUR DE TRAVAIL DECODAGE
!
    call jemarq()
    call dismoi('F', 'NB_CMP_MAX', nomres, 'INTERF_DYNA', nbcmp,&
                k8bid, iret)
    call dismoi('F', 'NB_EC', nomres, 'INTERF_DYNA', nbec,&
                k8bid, iret)
    temidc = '&&'//pgc//'.IDEC'
    call wkvect(temidc, 'V V I', nbcmp*nbec*2, ltidec)
!
!-----------REQUETTE ADRESSE DE LA TABLE DESCRIPTION DES DEFORMEES------
!
    desdef = nomres//'.IDC_DEFO'
    call jeveuo(desdef, 'E', lldes)
    call jelira(desdef, 'LONMAX', nbnot, k8bid)
    nbnot = nbnot/(2+nbec)
!
!---------------COMPTAGE DES NOEUDS DES DIVERS TYPES INTERFACE----------
!
    do 5 i = 1, 4
        ikyp(i) = 0
 5  end do
!
    do 10 i = 1, nbnot
        ik = zi(lldes+nbnot+i-1)
        ik = -ik
        ikyp(ik) = ikyp(ik) + 1
10  end do
!
    nomax = max(ikyp(1),ikyp(2))
    nomax = max(nomax,ikyp(3))
    nomax = max(nomax,ikyp(4))
!
!-----------CREATION MATRICE DES ENTIER CODES DDL ASSEMBLES------------
!    COLONNES SEPARES POUR LES DDL PHYSIQUES ET LES LAGRANGES
!
    nomax = 2*nomax*nbec
    temmat = '&&'//pgc//'.MATDDL'
    call wkvect(temmat, 'V V I', nomax, ltmat)
!
!--------------------REQUETE SUR LE DEEQ DU NUMDDL----------------------
!
!
!
    deeq = numddl//'.DEEQ'
    call jeveuo(deeq, 'L', lldeeq)
    call jelira(deeq, 'LONMAX', neq, k8bid)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8bid, iret)
!
!--------------TRAITEMENT DES MODES D'ATTACHE (MAC NEAL)----------------
!
!
!   DECALAGE EVENTUEL DE LA LISTE DES NOEUDS MN DANS LA LISTE GLOBALE
!
    nbtem = 0
    nbdef = 0
!
    nbno = ikyp(1)
!
    call recddl(nbcmp, zi(lldes+nbtem), nbno, nbec, zi(lldeeq),&
                neq, zi(ltmat), zi(ltidec))
!
    iad1 = lldes + nbnot*2 + nbtem*nbec
    iad2 = lldes + nbnot + nbtem
    call maskmn(nbcmp, nbno, nbec, zi(ltmat), zi(iad1),&
                zi(iad2), nbdef)
!
!----------TRAITEMENT DES MODES CONTRAINTS (CRAIG BAMPTON)--------------
!
!   DECALAGE EVENTUEL DE LA LISTE DES NOEUDS MN DANS LA LISTE GLOBALE
!
    nbtem = ikyp(1)
!
    nbno = ikyp(2)
!
    call recddl(nbcmp, zi(lldes+nbtem), nbno, nbec, zi(lldeeq),&
                neq, zi(ltmat), zi(ltidec))
!
    iad1 = lldes + nbnot*2 + nbtem*nbec
    iad2 = lldes + nbnot + nbtem
    call maskcb(nbcmp, nbno, nbec, zi(ltmat), zi(iad1),&
                zi(iad2), nbdef)
!
!-------TRAITEMENT DES MODES CONTRAINTS HARMONIQUES(CB-HARMO)-----------
!
!   DECALAGE EVENTUEL DE LA LISTE DES NOEUDS MN DANS LA LISTE GLOBALE
!
    nbtem = ikyp(1) + ikyp(2)
!
    nbno = ikyp(3)
!
    call recddl(nbcmp, zi(lldes+nbtem), nbno, nbec, zi(lldeeq),&
                neq, zi(ltmat), zi(ltidec))
!
    iad1 = lldes + nbnot*2 + nbtem*nbec
    iad2 = lldes + nbnot + nbtem
    call maskcb(nbcmp, nbno, nbec, zi(ltmat), zi(iad1),&
                zi(iad2), nbdef)
!
!-----------------TRAITEMENT DES NOEUDS D'INTERFACE AUCUN---------------
!
!
!   DECALAGE EVENTUEL DE LA LISTE DES NOEUDS AU DANS LA LISTE GLOBALE
!
    nbtem = ikyp(1) + ikyp(2) + ikyp(3)
!
    nbno = ikyp(4)
!
    call recddl(nbcmp, zi(lldes+nbtem), nbno, nbec, zi(lldeeq),&
                neq, zi(ltmat), zi(ltidec))
!
    iad1 = lldes + nbnot*2 + nbtem*nbec
    iad2 = lldes + nbnot + nbtem
    call maskau(nbno, nbec, zi(iad1))
!
!------------------------FINITION DU .DESC------------------------------
!
    call jeveuo(nomres//'.IDC_DESC', 'E', lddesc)
    zi(lddesc+4) = nbdef
!
!---------------------LIBERATION DES OBJETS-----------------------------
!
!
!-------------------DESTRUCTION DES OBJETS VOLATILES--------------------
!
    call jedetr(temmat)
    call jedetr(temidc)
!
    call jedema()
end subroutine
