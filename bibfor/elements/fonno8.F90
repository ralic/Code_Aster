subroutine fonno8(resu, noma, tablev, vnor, vect)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/panbno.h'
    character(len=8) :: noma, resu
    integer :: tablev(2)
    real(kind=8) :: vect(3), vnor(2, 3)
!
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
!       ----------------------------------------------------------------
!      DETERMINATION D'UN VECTEUR SE DIRIGEANT VERS LA LEVRE SUPERIEURE
!       ----------------------------------------------------------------
!    ENTREES
!       RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!       NOMA   : NOM DU MAILLAGE
!       TABLEV : VECTEUR CONTENANT LES NUMEROS DES DEUX MAILLES
!                CONNECTEES AU NOEUD SOMMET COURANT DU PREMIER SEGMENT
!                ET AUX LEVRES
!       VNOR   : VECTEUR NORMAL AU NOEUD SOMMET COURANT
!    SORTIE
!       VECT   : VECTEUR SE DIRIGEANT VERS LA LEVRE SUPERIEURE
!                IL SERT A REORIENTER LE VECTEUR NORMAL POUR QU'IL AILLE
!                DE LA LEVRE INF VERS LA LEVRE SUP
!
!
    integer :: comp
    integer :: iamase, iatyma, ifon, ilev, inn, inn2, inp, iret, ityp, itypma
    integer :: jcoor, jconx, jfon, jmale
    integer :: nblev, nn, nn2, nbnott(3)
    real(kind=8) :: xg, yg, zg
    character(len=8) :: k8b, type
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DES DONNES SUR LE MAILLAGE
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
    call jeveuo(resu//'.LEVRESUP.MAIL', 'L', jmale)
    call jelira(resu//'.LEVRESUP.MAIL', 'LONUTI', nblev, k8b)
!
!
!     DETERMINATION D'UN VECTEUR ALLANT DU PREMIER NOEUD DU FOND
!     FISSURE A UN NOEUD DE LA LEVRE_SUP
!
    do 200 inp = 1, 2
!
        call jeveuo(jexnum(noma//'.CONNEX', tablev(inp)), 'L', jconx)
        itypma = iatyma-1+tablev(inp)
!
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(itypma)), type)
        call dismoi('F', 'NBNO_TYPMAIL', type, 'TYPE_MAILLE', nn2,&
                    k8b, iret)
!
        do 210 ilev = 1, nblev
            comp = 0
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jmale-1+ilev)), iret)
            call jeveuo(jexnum(noma//'.CONNEX', iret), 'L', iamase)
            ityp = iatyma-1+iret
!
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
            call dismoi('F', 'NBNO_TYPMAIL', type, 'TYPE_MAILLE', nn,&
                        k8b, iret)
!
            do 220 inn = 1, nn
                do 230 inn2 = 1, nn2
                    if (zi(jconx-1+inn2) .eq. zi(iamase-1 + inn)) then
                        comp=comp+1
                        if (comp .eq. nn) goto 300
                    endif
230              continue
220          continue
210      continue
200  end do
!
300  continue
!
!     CALCUL DES COORDONNEES DU CENTRE DE GRAVITE
    call panbno(zi(itypma), nbnott)
!
    xg=0
    yg=0
    zg=0
    do 240 inn2 = 1, nbnott(1)
        xg=xg+zr(jcoor-1 + (zi(jconx-1+inn2)-1)*3 + 1)
        yg=yg+zr(jcoor-1 + (zi(jconx-1+inn2)-1)*3 + 2)
        zg=zg+zr(jcoor-1 + (zi(jconx-1+inn2)-1)*3 + 3)
240  end do
!
    call jeexin(resu//'.FOND.NOEU', ifon)
    if (ifon .ne. 0) then
        call jeveuo(resu//'.FOND.NOEU', 'L', jfon)
    else
        call jeveuo(resu//'.FOND_INF.NOEU', 'L', jfon)
    endif
    call jenonu(jexnom(noma//'.NOMNOE', zk8(jfon)), iret)
!
    vect(1) = xg/nbnott(1) - zr(jcoor-1 + (iret-1)*3 + 1)
    vect(2) = yg/nbnott(1) - zr(jcoor-1 + (iret-1)*3 + 2)
    vect(3) = zg/nbnott(1) - zr(jcoor-1 + (iret-1)*3 + 3)
!
    call jedema()
end subroutine
