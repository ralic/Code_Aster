subroutine caxfem(fonree, char)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/aflrch.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exixfe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/xrelco.h'
    character(len=8) :: char
    character(len=4) :: fonree
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION - AFFE_CHAR_MECA)
!
! CREER LES RELATIONS LINÉAIRES QUI ANNULENT LES DDLS EN TROP
!
! ----------------------------------------------------------------------
!
!
! IN  FONREE : FONC OU REEL SUIVANT L'OPERATEUR
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
!
!
!
    integer :: nfismx
    parameter    (nfismx=100)
    integer :: ibid, ier, i
    integer :: nfiss, nrel
    integer :: jfiss, jnfis, jxc
    character(len=19) :: lisrel
    character(len=8) :: rep
    character(len=8) :: noma, nomo, sdcont
    logical :: lcontx
    character(len=24) :: modcon
    integer :: jmoco
    character(len=16) :: valk(2)
    character(len=8) :: modelx
    character(len=24) :: xnrell, xnbasc
    integer :: jxnrel, jxnbas
    character(len=19) :: nliseq, nlisrl, nlisco, nbasco
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (fonree .ne. 'REEL') goto 9999
!
    rep = 'NON'
    call getvtx(' ', 'LIAISON_XFEM', 0, iarg, 1,&
                rep, ibid)
    if (rep .ne. 'OUI') goto 9999
!
! --- MAILLAGE ET MODELE
!
    call dismoi('F', 'NOM_MODELE', char(1:8), 'CHARGE', ibid,&
                nomo, ier)
    call dismoi('F', 'NOM_MAILLA', nomo, 'MODELE', ibid,&
                noma, ier)
!
! --- ACCES A LA SD FISSURE
!
    call exixfe(nomo, ier)
!
    if (ier .eq. 0) then
        valk(1) = nomo
        call u2mesk('F', 'XFEM2_12', 1, valk)
    endif
!
    call jeveuo(nomo(1:8)//'.FISS', 'L', jfiss)
    call jeveuo(nomo(1:8)//'.NFIS', 'L', jnfis)
    nfiss = zi(jnfis)
!
    if (nfiss .gt. nfismx) then
        call u2mesi('F', 'XFEM_2', 1, nfismx)
    endif
!
! --- ACCES SD_CONTACT SI LIAISON CONTACT
!
    call jeveuo(nomo(1:8)//'.XFEM_CONT', 'L', jxc)
    lcontx = zi(jxc) .ge. 1
    if (lcontx) then
        call getvid(' ', 'CONTACT_XFEM', 1, iarg, 1,&
                    sdcont, ier)
        if (ier .eq. 0) then
            call u2mess('F', 'XFEM2_7')
        else
            modcon = sdcont(1:8)//'.CONTACT.MODELX'
            call jeveuo(modcon, 'L', jmoco)
            modelx = zk8(jmoco)
            if (modelx .ne. nomo) then
                valk(1) = modelx
                valk(2) = sdcont(1:8)
                call u2mesk('F', 'XFEM2_11', 2, valk)
            else
                xnrell = sdcont(1:8)//'.CONTACT.XNRELL'
                call jeveuo(xnrell, 'L', jxnrel)
            endif
        endif
    endif
!
! --- INITIALISATIONS
!
!
    lisrel = '&&CAXFEM.RLISTE'
    nrel=0
!
! --- BOUCLE SUR LES FISSURES
!
    do 10 i = 1, nfiss
!
! --- RELATIONS ENTRE LES INCONNUES DE CONTACT (POUR LA LBB)
!
        if (lcontx) then
            nliseq = zk24(jxnrel+i-1)(1:19)
            call xrelco(noma, nliseq, lisrel, nrel)
        endif
!
10  end do
!
! --- AFFECTATION DES RELATIONS LINEAIRES DANS LE LIGREL DE CHARGE
!
    if (nrel .ne. 0) then
        call aflrch(lisrel, char)
    endif
!
9999  continue
    call jedema()
end subroutine
