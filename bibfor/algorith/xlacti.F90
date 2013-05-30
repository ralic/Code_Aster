subroutine xlacti(typma, ninter, jaint, lact, nlact)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/conare.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/xxmmvd.h'
    character(len=8) :: typma
    integer :: ninter, jaint, lact(8), nlact
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
! ACTIVATION DES LAGRANGES POUR LA FORMULATION LAGRANGES AUX NOEUDS
! POUR CHAQUE NOEUD SOMMET,
!        LACT(INO)=0: LES DDL DE CONTACT DE CE NOEUD NE SONT PAS ACTIFS
!        LACT(INO)=NLI > 0: LES DDL DE CONTACT DE CE SOMMET SONT ACTIFS
!                       NLI EST LE NUMEROS DU PT D'INTERSECTION
!
! POUR OPTIMISER, ON PEUT SORTIR XLACTI DES TE'S, IL FAUT ALORS
! PASSER UN CHAMP SUPPLÉMENTAIRE AUX TE'S
!
! ----------------------------------------------------------------------
!
! IN  TYPMA  : TYPE  DE MAILLE DE L'ELEMENT PARENT
! IN  NINTER : NOMBRE DE POINTS D'INTERSECTION
! IN  JAINT  : ADRESSE DES INFORMATIONS CONCERNANT LES ARETES COUPÉES
! OUT LACT   : LISTE DES LAGRANGES ACTIFS
! OUT NLACT  : NOMBRE TOTAL DE LAGRANGES ACTIFS
!
!
!
!
    integer :: zxain
    integer :: ino, ino1, ino2, iar, ar(12, 3), nbar
    integer :: vit(8), nvit, nli
!.......................................................................
    call jemarq()
!
! --- INITIALISATIONS
!
    do 30 ino = 1, 8
        lact(ino) = 0
        vit(ino) = 0
30  end do
    nlact = 0
    call conare(typma, ar, nbar)
    zxain=xxmmvd('ZXAIN')
!
! --- ON ACTIVE LES NOEUDS CONNECTES AUX POINTS D'INTERSECTION
    do 10 nli = 1, ninter
        iar=int(zr(jaint-1+zxain*(nli-1)+1))
        ino=int(zr(jaint-1+zxain*(nli-1)+2))
        nvit=int(zr(jaint-1+zxain*(nli-1)+5))
        if (ino .gt. 0) then
            lact(ino)=nli
        else if (iar.gt.0) then
            ino1=ar(iar,1)
            ino2=ar(iar,2)
            if (nvit .eq. 1) then
                lact(ino1)=nli
                vit(ino1)=1
                lact(ino2)=nli
                vit(ino2)=1
            else
                if (vit(ino1) .eq. 0) lact(ino1)=nli
                if (vit(ino2) .eq. 0) lact(ino2)=nli
            endif
        endif
10  end do
! --- ON COMPTE LE NOMBRE DE NOEUDS ACTIFS
    do 20 ino = 1, 8
        if (lact(ino) .ne. 0) nlact=nlact+1
20  end do
!
    call jedema()
end subroutine
