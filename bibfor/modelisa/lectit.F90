subroutine lectit(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nbg, dim,&
                  nbt, irteti)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       PREMIERE LECTURE DES DONNEES POUR  UN MOT CLE DE TYPE TITRE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL =  VOIR LIRITM
!               MCL             = MOTS CLES TYPE TITRE
!               NBG             = NIVEAU DEBUG
!               NBM             = NB DE MOTS CLES TYPE TITRE
!                               = 1 > ERREUR EN LECTURE
!               DIM             = DIMENSION  OBJET TITRE(NB LIGNES)
!               NBT             = NB TOTAL DE LIGNES LUES(ICI NBT=DIM)
!               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE)
!       ----------------------------------------------------------------
!
    include 'asterfort/iunifi.h'
    include 'asterfort/lirlig.h'
    include 'asterfort/lxscan.h'
    include 'asterfort/tesfin.h'
    include 'asterfort/tesmcl.h'
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    integer :: dim(nbm), nbt(nbm)
    character(len=14) :: cnl
    character(len=*) :: cv
    character(len=16) :: nop
    common          /opmail/        nop
    character(len=80) :: lig
!-----------------------------------------------------------------------
    integer :: icl, ideb, ifl, ifm, irtet, irteti
    integer :: iv, nbg, nbm
!-----------------------------------------------------------------------
    irteti = 0
!
    ifm = iunifi('MESSAGE')
!
! - ITEM = MOT CLE  TITRE  ?
!
    call tesmcl(icl, iv, cv, mcl(1), irtet)
    if (irtet .gt. 0) goto (3), irtet
    if (nbg .ge. 1) write(ifm,*)' ----- LECTIT'
!
! - LIRE LIGNE SUIVANTE
!
 4  continue
    call lirlig(ifl, cnl, lig, 1)
!
    if (nbg .ge. 1) write(ifm,*)'       LIRLIG :',cnl,lig
!
! - LIRE PREMIER ITEM DE LA LIGNE
!
    ideb = 1
    call lxscan(lig, ideb, icl, iv, rv,&
                cv)
!
    if (nbg .ge. 1) write(ifm, *)'       LXSCAN : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                    ' CV(1:8) = ', cv(1:8), ' IDEB = ', ideb
!
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .gt. 0) goto (1,2), irtet
    dim(1) = dim(1) + 1
    nbt(1) = nbt(1) + 1
!
    goto 4
!
 1  continue
    irteti = 1
    goto 9999
 2  continue
    irteti = 2
    goto 9999
 3  continue
    irteti = 0
    goto 9999
!
9999  continue
end subroutine
