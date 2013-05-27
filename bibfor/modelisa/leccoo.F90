subroutine leccoo(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nbg, dim,&
                  nbt, ier, irteti)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE COORDONNEE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLE TYPE COORDONNEE
!               NBG             = NIVEAU DEBUG
!               NBM             = NB DE MOTS CLES TYPE COORDONNEE
!       OUT     IER             = 0 > LECTURE CORRECTE
!                               = 1 > ERREUR EN LECTURE
!               DIM             = DIMENSION DE L OBJET COORDO (NB NOEU.)
!               NBT             = NB TOTAL DE COORDONNEES LUES
!               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
    include 'asterfort/iunifi.h'
    include 'asterfort/liritm.h'
    include 'asterfort/lirtet.h'
    include 'asterfort/tesfin.h'
    include 'asterfort/tesmcl.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/verdbl.h'
    include 'asterfort/vermot.h'
    include 'asterfort/vernmb.h'
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    integer :: dim(nbm), nbt(nbm), deblig
    character(len=14) :: cnl
    character(len=16) :: cmd
    common          /opmail/        cmd
    character(len=*) :: cv
    character(len=24) :: valk(3), nom
!-----------------------------------------------------------------------
    integer :: i, icl, ier, ifl, ifm, ilec, inom
    integer :: irtet, irteti, iv, j, nbg, nbm
    integer :: numtcl
!-----------------------------------------------------------------------
    irteti = 0
!
    ifm = iunifi('MESSAGE')
!
! ----- ITEM = MOT CLE  TYPE COORDONNEES ?
!
    do 4 i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .gt. 0) goto (4), irtet
        numtcl = i
        goto 5
 4  continue
    goto 3
!
! - VERIFICATION DE COMPATIBILITE DES DECLARATIONS DE DIMENSIONS
!
 5  continue
    if (nbg .ge. 1) write(ifm,*)' ----- LECCOO'
    do 6 j = 1, nbm
        if (dim(j) .ne. 0 .and. j .ne. i) then
            valk(1) = cnl
            valk(2) = mcl(i)
            valk(3) = mcl(j)
            call u2mesk('E', 'MODELISA4_77', 3, valk)
            ier = 1
            goto 2
        endif
 6  continue
!
! - LECTURE DE L'ENTETE
!
    inom=0
    ilec=1
    deblig=0
    call lirtet(ifl, ilec, inom, cnl, nom,&
                icl, iv, rv, cv, deblig)
    goto 9
!
! - LIRE ITEM SUIVANT
!
 7  continue
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 1)
    if (nbg .ge. 1) write(ifm, *)'       LIRITM : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                    ' CV(1:8) = ', cv(1:8), ' DEBLIG =', deblig
 9  continue
!
! - ITEM EN DEBUT DE LIGNE ?
!
    call verdbl(deblig, cnl, ier, irtet)
    if (irtet .gt. 0) goto (2), irtet
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .gt. 0) goto (1,2), irtet
!
! - ITEM = MOT ? > LECTURE NOM DU NOEUD
!
    call vermot(icl, iv, cv, cnl, ier,&
                irtet)
    if (irtet .gt. 0) goto (2), irtet
!
! - LECTURE DES COORDONNEES
!
    do 8 i = 1, numtcl
!
! - LIRE ITEM SUIVANT
!
        call liritm(ifl, icl, iv, rv, cv,&
                    cnl, deblig, 1)
        if (nbg .ge. 1) write(ifm, *)'       LIRITM : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                        ' CV(1:8) = ', cv(1:8), ' DEBLIG =', deblig
!
! - ITEM = NOMBRE ?
!
        call vernmb(icl, cnl, ier, irtet)
        if (irtet .gt. 0) goto (2), irtet
!
! - INCREMENTATION DU NB TOTAL DE COORDONNEES LUES
!
        nbt(numtcl) = nbt(numtcl) + 1
!
 8  continue
    dim(numtcl) = dim(numtcl) + 1
!
    goto 7
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
