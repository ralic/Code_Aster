subroutine utcovt(type, tbvalr, tbvali, tberr, rela,&
                  tchval, tcherr)
!
    implicit   none
    include 'asterfort/codent.h'
    include 'asterfort/codree.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/utfloa.h'
    include 'asterfort/utroun.h'
    character(len=1) :: type
    character(len=4) :: rela
    integer :: tbvali(2)
    real(kind=8) :: tbvalr(2), tberr(2)
    character(len=24) :: tchval(2)
    character(len=16) :: tcherr(2)
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
! ----------------------------------------------------------------------
! IN :
!    TYPE :  TYPE DES VALEURS ('R' POUR REEL, 'I' POUR ENTIER)
!    TBVALR : TABLEAU DES VALEURS REELS DE TEST (SI TYPE = 'R') :
!             TBVALR(1) : VALEUR DE REFERENCE
!             TBVALC(2) : VALEUR CALCULEE
!    RELA   : CRITERE 'RELATIF' OU 'ABSOLU'
!    TBVALI : SI TYPE = 'I' : IDEM TBVALR POUR LES REELS
!    TBERR  : TABLEAU DES TOLERANCE/ERREUR :
!             TBERR(1) : ERREUR
!             TBERR(2) : TOLERANCE
! OUT :
!    TCHVAL : TABLEAU DE CHAINES DE CARACTERES DES VALEURS FORMATEES
!             TCHVAL(1) : CHAINE REPRESENTANT LA VALEUR DE REFERENCE
!             TCHVAL(2) : CHAINE REPRESENTANT LA VALEUR CALCULEE
!    TCHERR : TABLEAU DE CHAINES DES ERREUR/TOLERANCE FORMATEES
!             TCHERR(1) : CHAINE REPRESENTANT L'ERREUR DE REFERENCE
!             TCHERR(2) : CHAINE REPRESENTANT LA TOLERANCE CALCULEE
!
!
! ----------------------------------------------------------------------
!
    integer :: lcvref, lcv, i, ii, exv, exvref, iret, ndec, v
!
    real(kind=8) :: sg, sgref
    character(len=24) :: chv, chvref, chv3, chv4, ch24, k24w
    character(len=7) :: siv, sivref
    logical :: valflo, refflo, errflo, tolflo
!
    call jemarq()
!
!
!----------------------------------------------------
!----- A) VALEUR CALCULEE & VALEUR DE REFERENCE -----
!----------------------------------------------------
!
    valflo=.false.
    refflo=.false.
    errflo=.false.
    tolflo=.false.
    siv = ' '
    sivref = ' '
    chv= ' '
    chvref = ' '
!
! --- 1 . CAS REEL
!     ============
!
    if (type(1:1) .eq. 'R') then
!
!  ---   ECRITURE AVEC OU SANS EXPOSANT
!        ------------------------------
!        VALFLO = TRUE : ECRITURE DE LA VALEUR CALCULEE SANS EXPOSANT
!        REFFLO = TRUE : ECRITURE DE LA VALEUR DE REF. SANS EXPOSANT
        if (abs(tbvalr(1)) .ge. 0.01d0 .and. abs(tbvalr(1)) .lt. 100000) then
            refflo=.true.
        endif
        if (abs(tbvalr(2)) .ge. 0.01d0 .and. abs(tbvalr(2)) .lt. 100000) then
            valflo=.true.
        endif
!
!  ---   PASSAGE : REELS => CHARACTERS:
!        ------------------------------
!        CHVREF : CHAINE REPRESENTANT LA VALEUR DE REFERENCE
!        CHV    : CHAINE REPRESENTANT LA VALEUR CALCULEE
        sgref=sign(1.0d0, tbvalr(1))
        call codree(sgref*tbvalr(1), 'E', chvref)
        lcvref = lxlgut(chvref)
!
        sg=sign(1.0d0, tbvalr(2))
        call codree(sg*tbvalr(2), 'E', chv)
        lcv = lxlgut(chv)
!
!
!  ---   POUR LA VALEUR CALCULEE, ON DETERMINE :
!        --------------------------------------
!        LE SIGNE DE L'EXPOSANT : SIV
!        LA VALEUR DE L'EXPOSANT : EXV
        ii=0
        do 20 i = 1, lcv
            if (chv(i:i) .ne. 'E') then
                ii=ii+1
                goto 20
            endif
            goto 21
20      continue
21      continue
!
        if (chv(ii+2:ii+2) .eq. '-') then
            siv='NEGATIF'
        else if (chv(ii+2:ii+2).eq.'+') then
            siv='POSITIF'
        endif
        call lxliis(chv(ii+3:ii+4), exv, iret)
!
! ---    POUR LA VALEUR DE REFERENCE, ON DETERMINE :
!        -------------------------------------------
!        LE SIGNE DE L'EXPOSANT : SIVREF
!        LA VALEUR DE L'EXPOSANT : EXVREF
        ii=0
        do 30 i = 1, lcvref
            if (chvref(i:i) .ne. 'E') then
                ii=ii+1
                goto 30
            endif
            goto 31
30      continue
31      continue
        if (chvref(ii+2:ii+2) .eq. '-') then
            sivref='NEGATIF'
        else if (chvref(ii+2:ii+2).eq.'+') then
            sivref='POSITIF'
        endif
        call lxliis(chvref(ii+3:ii+4), exvref, iret)
!
!
! ---    DETERMINATION DU NOMBRE DE DECIMALES A PRENDRE EN COMPTE:
!        ---------------------------------------------------------
!
!    --  TRAITEMENT DES CAS PARTICULIERS :
!
!       - SI LES SIGNES DES EXPOSANTS DIFFERENT,ON CONSIDERE 6 DECIMALES
        if (sivref(1:1) .ne. siv(1:1)) then
            ndec=6
            goto 555
        endif
!
!       - SI LES SIGNES DIFFERENT, ON PREND EN COMPTE 6 DECIMALES
        if (sg*sgref .lt. 0.d0) then
            ndec=6
            goto 555
        endif
!
!        - SI LES EXPOSANTS DIFFERENT, ON PREND EN COMPTE 6 DECIMALES
        if (exv .ne. exvref) then
            ndec=6
            goto 555
        endif
!
!
!    --  POSITION DE LA 1ERE DECIMALE DIFFERENTE (REFERENCE/CALCULEE): V
!
        ii=0
        do 10 i = 1, lcv
            if (chv(i:i) .eq. chvref(i:i)) then
                ii=ii+1
                goto 10
            endif
            goto 11
10      continue
11      continue
!
!        SI LE PREMIER CHIFFRE DIFFERE, ON PREND EN COMPTE 6 DECIMALES
        if (ii .eq. 0) then
            ndec=6
            goto 555
        endif
        v=ii-1
!
!
!    --  NOMBRE DE DECIMALES A PRENDRE EN COMPTE: NDEC
!
!       - SI L'EXPOSANT EST POSITIF:
!         IF(SIV.EQ.'POSITIF')THEN
!            NDEC=MIN(12,MAX(V,EXV)+2)
!
!       - SI L'EXPOSANT EST NEGATIF:
!         ELSEIF(SIV.EQ.'NEGATIF')THEN
!           NDEC=MIN(12,V+2)
!
!         ENDIF
        ndec=min(14,v+2)
!
555      continue
!
!        -- FINALEMENT, DANS TOUS LES CAS DE FIGURE, ON FORCE NDEC=13
!           (VOIR FICHE DE REX 14850)
        ndec=13
!
!
!
!  ---   TRONCATURES:
!        ------------
!
!   --   POUR LA VALEUR CALCULEE:
        chv4=' '
        if (sg .lt. 0.d0) then
            k24w = '-'//chv(1:23)
            call utroun(k24w, ndec, chv4, 0)
        else
            call utroun(chv, ndec, chv4, 0)
        endif
!
!   --   POUR LA VALEUR DE REFERENCE:
        chv3=' '
        if (sgref .lt. 0.d0) then
            k24w = '-'//chvref(1:23)
            call utroun(k24w, ndec, chv3, 0)
        else
            call utroun(chvref, ndec, chv3, 0)
        endif
!
!
!  ---   ECRITURE EVENTUELLE SANS EXPOSANT:
!        ----------------------------------
!
!   --   POUR LA VALEUR DE REFERENCE:
        call utfloa(refflo, chv3, tchval(1))
!
!   --   POUR LA VALEUR CALCULEE:
        call utfloa(valflo, chv4, tchval(2))
!
!
! --- 2 . CAS ENTIER
!     ==============
!
    else if (type(1:1).eq.'I') then
!
        call codent(tbvali(1), 'G', tchval(1))
        call codent(tbvali(2), 'G', tchval(2))
!
    endif
!
!
!----------------------------------------------
!----- B) ERREUR & TOLERANCE -----------------
!----------------------------------------------
!
!
! --- ECRITURE AVEC OU SANS EXPOSANT
!     ------------------------------
!     ERRFLO = TRUE : ECRITURE DE L'ERREUR SANS EXPOSANT
!     TOLFLO = TRUE : ECRITURE DE LA TOLERANCE SANS EXPOSANT
    if (abs(tberr(1)) .ge. 0.01d0 .and. abs(tberr(1)) .lt. 100000) then
        errflo=.true.
    endif
    if (abs(tberr(2)) .ge. 0.01d0 .and. abs(tberr(2)) .lt. 100000) then
        tolflo=.true.
    endif
!
! --- POUR L'ERREUR CALCULEE:
    ch24=' '
    call codree(abs(tberr(1)), 'E', chv3)
    call utroun(chv3, 2, chv4, 0)
    call utfloa(errflo, chv4, ch24)
!
    if (rela .eq. 'RELA') then
        lcv = lxlgut(ch24)
        tcherr(1)=ch24(1:lcv)//'%'
    else
        tcherr(1)=ch24(1:16)
    endif
!
! --- POUR LA TOLERANCE:
    call codree(abs(tberr(2)), 'E', chv3)
    call utroun(chv3, 3, chv4, 0)
    call utfloa(tolflo, chv4, ch24)
!
    if (rela .eq. 'RELA') then
        lcv = lxlgut(ch24)
        tcherr(2)=ch24(1:lcv)//'%'
    else
        tcherr(2)=ch24(1:16)
    endif
!
!
    call jedema()
!
end subroutine
