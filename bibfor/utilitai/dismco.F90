subroutine dismco(questi, nomob, repi, repk, ierd)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
!
    integer :: repi, ierd
    character(len=*) :: nomob, repk
    character(len=*) :: questi
!
!
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
!     --     DISMOI(COMPOR)
!
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE CARTE_COMPOR
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
! ======================================================================
!
    character(len=19) :: chtmp, chcalc
    integer :: iret, jcalv, jcald, jcall, jcalk, nbma, ima, iadc
    character(len=6) :: lcham(3)
    character(len=8) :: noma, nomail
    logical :: incr, elas
    data  lcham/ 'RELCOM', 'DEFORM', 'INCELA'/
!
!
    call jemarq()
!
    call assert(questi(1:9).eq.'ELAS_INCR')
!
    repk = ' '
    repi = 0
    ierd = 0
!
    incr = .false.
    elas = .false.
!
    chtmp ='&&DISMCO_CHTMP'
    chcalc='&&GVERLC_CHCALC'
!
!     PASSAGE CARTE COMPOR --> CHAMP SIMPLE,
!     PUIS REDUCTION DU CHAMP SUR LA COMPOSANTE 'RELCOM'
!     QUI CORRESPOND AU NOM DE LA LOI DE COMPORTEMENT
!
    call carces(nomob, 'ELEM', ' ', 'V', chtmp,&
                'A', iret)
    call cesred(chtmp, 0, 0, 3, lcham,&
                'V', chcalc)
    call detrsd('CHAM_ELEM_S', chtmp)
!
    call jeveuo(chcalc//'.CESD', 'L', jcald)
    call jeveuo(chcalc//'.CESV', 'L', jcalv)
    call jeveuo(chcalc//'.CESL', 'L', jcall)
    call jeveuo(chcalc//'.CESK', 'L', jcalk)
!
    noma = zk8(jcalk-1+1)
    nbma = zi(jcald-1+1)
!
    do 10 ima = 1, nbma
!
        if (incr .and. elas) goto 999
!
        call cesexi('C', jcald, jcall, ima, 1,&
                    1, 1, iadc)
!
        if (iadc .gt. 0) then
            call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
            if (zk16(jcalv+iadc-1+2)(1:9) .eq. 'COMP_INCR') then
                incr = .true.
            endif
            if (zk16(jcalv+iadc-1+2)(1:9) .eq. 'COMP_ELAS') then
                elas = .true.
            endif
        endif
!
10  end do
!
999  continue
!
    if (incr .and. .not.elas) repk='INCR'
    if (elas .and. .not.incr) repk='ELAS'
    if (elas .and. incr) repk='MIXTE'
!
    if (.not.elas .and. .not.incr) ierd = 1
!
    call jedema()
!
end subroutine
