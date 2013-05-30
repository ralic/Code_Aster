subroutine nmceni(numedd, depdel, deppr1, deppr2, rho,&
                  sdpilo, eta, isxfe, f)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: numedd
    character(len=19) :: sdpilo, depdel, deppr1, deppr2
    real(kind=8) :: eta, rho, f
    logical :: isxfe
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - SELECTION PARAMETRE)
!
! CALCUL DU PARAMETRE DE SELECTION DE TYPE NORM_INCR_DEPL
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL
! IN  SDPILO : SD PILOTAGE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
! IN  DEPPR1 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
! IN  DEPPR2 : INCREMENT DE DEPLACEMENT K-1.F_PILO
! IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  ISXFE  : INDIQUE S'IL S'AGIT D'UN MODELE XFEM
! OUT F      : VALEUR DU CRITERE
!
!
!
!
    character(len=8) :: k8bid
    integer :: jdepde, jdu0, jdu1, ideeq, jcoee, jcoef
    character(len=19) :: profch, chapil, chapic
    integer :: neq, iret, i, j, ibid
    real(kind=8) :: dn, dc, dp
!
! ----------------------------------------------------------------------
!
    call jemarq()
    if (isxfe) then
        chapil = sdpilo(1:14)//'.PLCR'
        call jeveuo(chapil(1:19)//'.VALE', 'L', jcoef)
        chapic = sdpilo(1:14)//'.PLCI'
        call jeveuo(chapic(1:19)//'.VALE', 'L', jcoee)
    endif
!
! --- INITIALISATIONS
!
    f = 0.d0
!
! --- INFORMATIONS SUR NUMEROTATION
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    call dismoi('F', 'PROF_CHNO', depdel, 'CHAM_NO', ibid,&
                profch, iret)
    call jeveuo(profch(1:19)//'.DEEQ', 'L', ideeq)
!
! --- ACCES AUX VECTEURS SOLUTIONS
!
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
    call jeveuo(deppr1(1:19)//'.VALE', 'L', jdu0)
    call jeveuo(deppr2(1:19)//'.VALE', 'L', jdu1)
!
!
! --- CALCUL DE LA NORME
!
    if (isxfe) then
        do 20 i = 1, neq
            if (zi(ideeq-1 + 2*i ) .gt. 0) then
                if (zr(jcoee+i-1) .eq. 0.d0) then
                    f = f + zr(jcoef+i-1)**2* (zr(jdepde-1+i)+rho*zr( jdu0-1+i)+ eta*zr(jdu1-1+i)&
                        &)**2
                else
                    dn = 0.d0
                    dc = 0.d0
                    dp = 0.d0
                    do 31 j = i+1, neq
                        if (zr(jcoee+i-1) .eq. zr(jcoee+j-1)) then
                            dn = dn + zr(jcoef+i-1)*zr(jdepde+i-1)+ zr(jcoef+j-1)*zr(jdepde+j-1)
                            dc = dc + zr(jcoef+i-1)*zr(jdu0-1+i)+ zr(jcoef+j-1)*zr(jdu0-1+j)
                            dp = dp + zr(jcoef+i-1)*zr(jdu1-1+i)+ zr(jcoef+j-1)*zr(jdu1-1+j)
                        endif
31                  continue
                    f = f + (dn+rho*dc+eta*dp)**2
                endif
            endif
20      end do
    else
        do 30 i = 1, neq
            if (zi(ideeq-1 + 2*i + 2) .gt. 0) then
                f = f + (zr(jdepde+i)+rho*zr(jdu0+i)+eta*zr(jdu1+i))** 2
            endif
30      continue
    endif
    call jedema()
end subroutine
