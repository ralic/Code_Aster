subroutine nmarnr(result, typtaz, numreu)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/tbexve.h'
    character(len=8) :: result
    character(len=*) :: typtaz
    integer :: numreu
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (ARCHIVAGE)
!
! RECUPERATION NUMERO DE REUSE COURANT POUR LA TABLE
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  TYPTAB : TYPE DE LA TABLE
! OUT NUMREU : NUMERO DE REUSE POUR LA TABLE
!
! ----------------------------------------------------------------------
!
    integer :: iret, jtbnp, nblign
    character(len=19) :: nomtab
    character(len=16) :: typtab
    character(len=2) :: typvar
    logical :: lexist
    character(len=19) :: lisres
    integer :: jlisre, nval, ival, vali
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numreu = -1
    lisres = '&&NMARNR.NUME_REUSE'
    typtab = typtaz
!
! --- RECUPERATION DE LA LISTE DE TABLES SI ELLE EXISTE
!
    call jeexin(result//'           .LTNT', iret)
    if (iret .eq. 0) then
        numreu = 0
        goto 99
    endif
!
! --- RECUPERATION DU NOM DE LA TABLE
!
    nomtab = ' '
    call ltnotb(result, typtab, nomtab)
!
! --- LA TABLE EXISTE-T-ELLE ?
!
    call exisd('TABLE', nomtab, iret)
    if (iret .eq. 0) then
        numreu = 0
        goto 99
    else
        call tbexip(nomtab, 'NUME_REUSE', lexist, typvar)
        if (.not.lexist .or. typvar .ne. 'I') call assert(.false.)
!
! ----- NOMBRE DE LIGNES
!
        call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
        nblign = zi(jtbnp+1)
        if (nblign .eq. 0) then
            numreu = 0
            goto 99
        endif
!
! ----- EXTRACTION DE LA COLONNE 'NUME_REUSE' DANS UN OBJET TEMPORAIRE
!
        call tbexve(nomtab, 'NUME_REUSE', lisres, 'V', nval,&
                    typvar)
        call jeveuo(lisres, 'L', jlisre)
!
! ----- RECUPERATION DU MAX
!
        do 10 ival = 1, nval
            vali = zi(jlisre-1+ival)
            if (vali .gt. numreu) numreu = vali
10      continue
        numreu = numreu + 1
    endif
!
99  continue
!
    call jedetr(lisres)
    call assert(numreu.ge.0)
!
    call jedema()
end subroutine
