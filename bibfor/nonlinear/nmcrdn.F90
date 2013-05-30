subroutine nmcrdn(sdsuiv, motfac, ntsddl, nbocc)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/impfoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: ntsddl, nbocc
    character(len=24) :: sdsuiv
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SUIVI_DDL)
!
! LECTURE NOM DES COLONNES
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  SDSUIV : NOM DE LA SD POUR SUIVI_DDL
! IN  NTSDDL : NOMBRE TOTAL DE SUIVI_DDL
! IN  NBOCC  : NOMBRE D'OCCURRENCES DE MOTFAC
!
! ----------------------------------------------------------------------
!
    integer :: iocc, isuiv, nbtit, ibid
    character(len=24) :: ddltit
    integer :: jddlti
    character(len=16) :: k16bid
    character(len=16) :: titre(3)
    character(len=1) :: chaine
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- SD POUR SAUVER LES TITRES
!
    ddltit = sdsuiv(1:14)//'     .TITR'
    call wkvect(ddltit, 'V V K16', 3*ntsddl, jddlti)
!
! --- TITRE DONNE PAR L'UTILISATEUR
!
    do 10 iocc = 1, nbocc
        call impfoi(0, 1, iocc, chaine)
        titre(1) = '    SUIVI '
        titre(2) = '     DDL  '
        titre(3) = '     '//chaine
        call getvtx(motfac, 'TITRE', iocc, iarg, 0,&
                    k16bid, nbtit)
        nbtit = - nbtit
        call assert(nbtit.le.3)
        if (nbtit .ne. 0) then
            call getvtx(motfac, 'TITRE', iocc, iarg, nbtit,&
                        titre, ibid)
        endif
        zk16(jddlti+3*(iocc-1)+1-1) = titre(1)
        zk16(jddlti+3*(iocc-1)+2-1) = titre(2)
        zk16(jddlti+3*(iocc-1)+3-1) = titre(3)
10  end do
!
! --- COMPLETUDE: TITRE AUTOMATIQUE
!
    if (ntsddl .gt. nbocc) then
        do 15 isuiv = nbocc+1, ntsddl
            call impfoi(0, 1, isuiv, chaine)
            titre(1) = '    SUIVI '
            titre(2) = '     DDL  '
            titre(3) = '     '//chaine
            zk16(jddlti+3*(isuiv-1)+1-1) = titre(1)
            zk16(jddlti+3*(isuiv-1)+2-1) = titre(2)
            zk16(jddlti+3*(isuiv-1)+3-1) = titre(3)
15      continue
    endif
!
    call jedema()
end subroutine
