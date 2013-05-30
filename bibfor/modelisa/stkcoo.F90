subroutine stkcoo(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, num, coo,&
                  nno, irteti)
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
!       SECONDE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE COORDONNEE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLES TYPE COORDONNEE
!               NBM             = NB DE MOTS CLES TYPE COORDONNEE
!               COO             = NOMU.COORDO.VALE
!               NNO             = NOMU.NOMNOE
!               NUM             = NUMERO DU NOEUD COURANT
!       OUT     (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/liritm.h'
    include 'asterfort/lirtet.h'
    include 'asterfort/tesfin.h'
    include 'asterfort/tesmcl.h'
    include 'asterfort/u2mesk.h'
    integer :: deblig
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm), nomn
    character(len=14) :: cnl
    character(len=*) :: cv
    character(len=24) :: coo, nno, nom
!
!-----------------------------------------------------------------------
    integer :: i, iad, icl, idec, ifl, iret
    integer :: irtet, irteti, iv, nbm, num, numtcl
!
!-----------------------------------------------------------------------
    call jemarq()
    irteti = 0
!
!
! - ITEM = MOTS CLES  TYPE COORDONNEES ?
!
    do 4 i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .gt. 0) goto (4), irtet
        numtcl = i
        goto 5
 4  continue
    goto 3
!
 5  continue
    call jeveuo(coo, 'E', iad)
!
! - LECTURE DE L'ENTETE
!
    deblig=0
    call lirtet(ifl, 2, 0, cnl, nom,&
                icl, iv, rv, cv, deblig)
    goto 9
!
! - LIRE ITEM SUIVANT =  NOM DU NOEUD ?
!
 7  continue
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 2)
 9  continue
!
! - ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .gt. 0) goto (1,2), irtet
!
! - CREATION DE NOM_DU_NOEUD DANS LE REPERTOIRE NOMNOE
!
    nomn = '        '
    nomn(1:iv) = cv(1:iv)
    call jeexin(jexnom(nno, nomn), iret)
    if (iret .eq. 0) then
        call jecroc(jexnom(nno, nomn))
    else
        call u2mesk('F', 'MODELISA7_10', 1, nomn)
    endif
!
! - INCREMENTATION NUMERO DU NOEUD
!
    num = num + 1
    idec = iad + (num-1) * 3
!       IDEC = IAD + (NUM-1) * NUMTCL
!
! - STOCKAGE DES  COORDONNEES DU NOEUD
!
    do 10 i = 1, 3
        zr(idec+i-1) = 0.d0
10  continue
!
    do 6 i = 1, numtcl
        call liritm(ifl, icl, iv, rv, cv,&
                    cnl, deblig, 2)
        if(icl.eq.1)rv = iv
        zr(idec+i-1) = rv
 6  continue
!
! - NOEUD SUIVANT
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
    call jedema()
end subroutine
