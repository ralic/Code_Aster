subroutine crpcvg(ma1, ma2, gma1, gma2, tran,&
                  prec, lima1, lima2, linoeu)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    real(kind=8) :: tran(3), prec
    integer :: linoeu(*)
    character(len=8) :: ma1, ma2
    character(len=24) :: gma1, gma2
    character(len=*) :: lima1, lima2
! ----------------------------------------------------------------------
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
!
!     COMMANDE:  CREA_RESU
!     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAMP"
!
! ----------------------------------------------------------------------
!
!
!
    integer :: nbma1, nbma2, jtyma1, jtyma2, jgma1, jgma2, ima, ima1, ima2, ino
    integer :: ino1, ino2, nbnoma, nutyp1, numgl1, iamac1, ilmac1, nutyp2
    integer :: numgl2, iamac2, ilmac2, jcoor1, jcoor2, jnum1, jnum2, jma
    real(kind=8) :: x1, y1, z1, x2, y2, z2, v1, v2, v3
    real(kind=8) :: valr(3)
    logical :: erreur
    character(len=8) :: k8b, noma1, noma2
    character(len=24) :: valk(4)
    character(len=24) :: grpma1, grpma2, coova1, coova2, typma1, typma2, conne1
    character(len=24) :: conne2
!
!     NBNOMA(IMA) = NOMBRE DE NOEUDS DE LA MAILLE IMA
    nbnoma(ima) = zi(ilmac1-1+ima+1) - zi(ilmac1-1+ima)
!
!     NUMGLM(IMA,INO) = NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
    numgl1(ima,ino) = zi(iamac1-1+zi(ilmac1+ima-1)+ino-1)
    numgl2(ima,ino) = zi(iamac2-1+zi(ilmac2+ima-1)+ino-1)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    grpma1 = ma1//'.GROUPEMA       '
    grpma2 = ma2//'.GROUPEMA       '
    coova1 = ma1//'.COORDO    .VALE'
    coova2 = ma2//'.COORDO    .VALE'
    typma1 = ma1//'.TYPMAIL        '
    typma2 = ma2//'.TYPMAIL        '
    conne1 = ma1//'.CONNEX         '
    conne2 = ma2//'.CONNEX         '
!
    call jeveuo(coova1, 'L', jcoor1)
    call jeveuo(coova2, 'L', jcoor2)
!
    call jeveuo(typma1, 'L', jtyma1)
    call jeveuo(typma2, 'L', jtyma2)
!
    call jeveuo(conne1, 'L', iamac1)
    call jeveuo(jexatr(conne1, 'LONCUM'), 'L', ilmac1)
    call jeveuo(conne2, 'L', iamac2)
    call jeveuo(jexatr(conne2, 'LONCUM'), 'L', ilmac2)
!
    call jelira(jexnom(grpma1, gma1), 'LONUTI', nbma1, k8b)
    call jelira(jexnom(grpma2, gma2), 'LONUTI', nbma2, k8b)
    if (nbma1 .ne. nbma2) then
        valk (1) = gma1
        valk (2) = gma2
        call u2mesg('F', 'CALCULEL5_67', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    call jeveuo(jexnom(grpma1, gma1), 'L', jgma1)
    call jeveuo(jexnom(grpma2, gma2), 'L', jgma2)
!
    call wkvect(lima1, 'V V I', nbma1, jnum1)
    call wkvect(lima2, 'V V I', nbma1, jnum2)
!
    do 10 ima = 1, nbma1
!
        ima1 = zi(jgma1+ima-1)
        nutyp1 = zi(jtyma1-1+ima1)
        do 100 jma = 1, nbma1
            ima2 = zi(jgma2+jma-1)
            nutyp2 = zi(jtyma2-1+ima2)
!
            do 20 ino = 1, nbnoma(ima1)
                ino1 = numgl1(ima1,ino)
                ino2 = numgl2(ima2,ino)
                x1 = zr(jcoor1-1+3*(ino1-1)+1)
                y1 = zr(jcoor1-1+3*(ino1-1)+2)
                z1 = zr(jcoor1-1+3*(ino1-1)+3)
                x2 = zr(jcoor2-1+3*(ino2-1)+1)
                y2 = zr(jcoor2-1+3*(ino2-1)+2)
                z2 = zr(jcoor2-1+3*(ino2-1)+3)
                v1 = abs ( x2 - x1 - tran(1) )
                v2 = abs ( y2 - y1 - tran(2) )
                v3 = abs ( z2 - z1 - tran(3) )
                erreur = .false.
                if (v1 .gt. prec) erreur = .true.
                if (v2 .gt. prec) erreur = .true.
                if (v3 .gt. prec) erreur = .true.
                if (erreur) then
                    goto 100
                endif
!
                linoeu(ino2) = ino1
!
20          continue
!
            if (nutyp1 .ne. nutyp2) then
                valk (1) = gma1
                valk (2) = gma2
                call u2mesg('F', 'CALCULEL5_68', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
!
            zi(jnum1+ima-1) = ima1
            zi(jnum2+ima-1) = ima2
            goto 10
100      continue
!
        call jenuno(jexnum(ma1//'.NOMMAI', ima1 ), noma1)
        call jenuno(jexnum(ma2//'.NOMMAI', ima2 ), noma2)
        valk(1) = noma1
        valk(2) = ma1
        valk(3) = noma2
        valk(4) = ma2
        valr(1) = tran(1)
        valr(2) = tran(2)
        valr(3) = tran(3)
        call u2mesg('F', 'CALCULEL5_69', 4, valk, 0,&
                    0, 1, valr)
10  end do
!
    call jedema()
end subroutine
