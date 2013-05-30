subroutine mdfred(nbmode, depgen, fexgen, nbrede, dplred,&
                  parred, fonred, saured, saredi)
    implicit none
! ----------------------------------------------------------------------
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
! CALCUL DU SECOND MEMBRE POUR UNE NON-LINEARITE DE TYPE RELA_EFFO_DEPL
! ----------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DEPGEN : DEPLACEMENTS GENERALISES AU PAS COURANT
! VAR : FEXGEN : FORCES GENERALISEES AU PAS COURANT
! IN  : NBREDE : NOMBRE DE POINTS DECRIVANT LA NON-LINEARITE
! IN  : DPLRED : TABLEAU DES DEPLACEMENTS MODAUX AUX NOEUDS DE RED
! IN  : PARRED : PARAMETRES DES NON-LINEARITES
! IN  : FONRED : FONCTIONS DE NON-LINEARITE
! OUT : SAURED : VALEURS SAUVEGARDEES
! OUT : SAREDI : VALEURS SAUVEGARDEES
! ----------------------------------------------------------------------
    include 'asterfort/fointe.h'
    integer :: ierd, icomp, nbmode, nbrede, saredi(*)
    real(kind=8) :: resu, seuil, forc, xs, absl, penl, depgen(*), fexgen(*)
    real(kind=8) :: parred(nbrede, *), saured(*), dplred(nbrede, nbmode, *)
    character(len=8) :: fonc, comp, fonred(nbrede, *)
    integer :: i, j
!-----------------------------------------------------------------------
!
!     --- BOUCLE SUR LES NOEUDS DE NON-LINEARITE ---
!
    do 10 i = 1, nbrede
!
        comp = fonred(i,2)
        fonc = fonred(i,3)
        absl = parred(i,1)
        penl = parred(i,2)
!
        if (comp(1:2) .eq. 'DX') icomp = 1
        if (comp(1:2) .eq. 'DY') icomp = 2
        if (comp(1:2) .eq. 'DZ') icomp = 3
        if (comp(1:3) .eq. 'DRX') icomp = 4
        if (comp(1:3) .eq. 'DRY') icomp = 5
        if (comp(1:3) .eq. 'DRZ') icomp = 6
!
        seuil = 0.d0
        do 20 j = 1, nbmode
            seuil = seuil + dplred(i,j,icomp)*depgen(j)
20      continue
!
        saured(i) = seuil
        saredi(i) = 0
!
        xs = abs(seuil)
        if (xs .gt. absl) then
            saredi(i) = 1
            if (fonred(i,4) .eq. 'TRANSIS ') then
                call fointe('F ', fonc, 1, comp, xs,&
                            resu, ierd)
                if (seuil .lt. 0.d0) resu = -resu
                forc = ( penl * seuil ) - resu
!
                do 30 j = 1, nbmode
                    fexgen(j)=fexgen(j)+dplred(i,j,icomp)*forc
30              continue
!
            endif
        endif
!
        if (fonred(i,4) .eq. 'DEPL    ') then
            call fointe('F ', fonc, 1, comp, seuil,&
                        forc, ierd)
!
            do 40 j = 1, nbmode
                fexgen(j)=fexgen(j)+dplred(i,j,icomp)*forc
40          continue
        endif
!
10  end do
!
end subroutine
