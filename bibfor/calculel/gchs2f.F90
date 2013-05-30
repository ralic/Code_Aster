subroutine gchs2f(char1, char2, char3)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/indik8.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/nbec.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: char1, char2, char3
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
!     BUT : TRANSFORME :
!              CHARGE 'SCALAIRE' --> CHARGE 'FONCTION' (CONSTANTE)
!
!           (ROUTINE SPECIFIQUE A L'OPERATEUR CALC_G,
!            APPELEE PAR GCHARF, DONT LE BUT EST DE
!            FUSIONNER CHAR1 ET CHAR2)
!
!     IN       :    CHAR1  :  CHARGE 'SCALAIRE'
!              :    CHAR2  :  CHARGE 'FONCTION'
!     IN/OUT   :    CHAR3  :  CHARGE 'FONCTION'
!
! ======================================================================
! ----------------------------------------------------------------------
    integer :: jdes1, jdes2, jdes3, ncmp1, ncmp2, i, jval1, jval3, k, izo, jfpro
    integer :: jfval, kk, iec, reste, code, jncmp1, jncmp2, j, nec, ior
    real(kind=8) :: epsi
    character(len=8) :: k8b, nocmp1, nomfon
    character(len=19) :: nomf19
!
    call jemarq()
!
    nomfon=char3
    epsi = r8prem()
!
!     DUPLICATION AVANT MISE A JOUR
    call jedupo(char1//'.DESC', 'V', char3//'.DESC', .false.)
    call jedupo(char1//'.NOMA', 'V', char3//'.NOMA', .false.)
    call jedupo(char1//'.NOLI', 'V', char3//'.NOLI', .false.)
    call jedupo(char1//'.LIMA', 'V', char3//'.LIMA', .false.)
!
!     DESC (MAJ 1/2)
    call jeveuo(char1//'.DESC', 'L', jdes1)
    call jeveuo(char2//'.DESC', 'L', jdes2)
    call jeveuo(char3//'.DESC', 'E', jdes3)
    zi(jdes3)=zi(jdes2)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', zi(jdes1)), 'LONMAX', ncmp1, k8b)
    call jelira(jexnum('&CATA.GD.NOMCMP', zi(jdes2)), 'LONMAX', ncmp2, k8b)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', zi(jdes1)), 'L', jncmp1)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', zi(jdes2)), 'L', jncmp2)
!
!     VALE
    call jeveuo(char1//'.VALE', 'L', jval1)
    call wkvect(char3//'.VALE', 'V V K8', ncmp2*zi(jdes3+3-1), jval3)
!
    k=0
    kk=0
    do 10 i = 1, ncmp1
        nocmp1 = zk8(jncmp1-1+i)
        j=indik8(zk8(jncmp2),nocmp1,1,ncmp2)
        if (j .ne. 0) then
            k=k+1
            if (k .eq. 1) then
                iec = (i-1)/30 + 1
                reste = i - 30* (iec-1)
                code = 2**reste
            endif
            do 20 izo = 1, zi(jdes1+3-1)
                if (abs(zr(jval1+(izo-1)*ncmp1+i-1)) .gt. epsi) then
                    kk=kk+1
                    call codent(kk, 'D0', nomfon(7:8))
                    nomf19=nomfon
                    call assert(lxlgut(nomf19).le.24)
                    call wkvect(nomf19//'.PROL', 'V V K24', 6, jfpro)
                    zk24(jfpro )='CONSTANT'
                    zk24(jfpro+1)='LIN LIN'
                    zk24(jfpro+2)='TOUTPARA'
                    zk24(jfpro+3)='TOUTRESU'
                    zk24(jfpro+4)='CC'
                    zk24(jfpro+5)=nomf19
                    call wkvect(nomf19//'.VALE', 'V V R', 2, jfval)
                    zr(jfval)=1.d0
                    zr(jfval+1)=zr(jval1+(izo-1)*ncmp1+i-1)
                    zk8(jval3+(izo-1)*ncmp2+j-1)=nomfon
                else
                    zk8(jval3+(izo-1)*ncmp2+j-1)='&FOZERO'
                endif
20          continue
        endif
10  end do
!
!     DESC (MAJ 2/2)
    nec = nbec(zi(jdes3))
    do 30 i = 1, nec*zi(jdes3+3-1)
        zi(jdes3+3+2*zi(jdes3+3-1)+i-1)= ior(zi(jdes3+3+2*zi(jdes3+3-&
        1)+i-1),code)
30  end do
!
    call jedema()
!
end subroutine
