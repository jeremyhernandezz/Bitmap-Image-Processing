#data starts at memory address 0x10000000 while Bitmap Display must be set to memory address 0x10008000 ($gp) such that the two don't overlap

.data 0x10000000
loading: .asciiz "Loading image, please wait for options...\n"
contrastOptions: .asciiz "Enter level of contrast 1-10\n"
saturationOptions: .asciiz "Enter the number for selection of saturation:\n0- Desaturation.\n1- Saturation.\nEnter:"
brightnessOptions: .asciiz "Enter level of brightness to adjust\nNegative number for darkening & for brightening, positive\nEnter a number from -10 to 10\n"
scaleImageOptions: .asciiz "Enter scaling factor to adjust size of image (1 for 64*64, 2 for 128*128, 3 for 256*256, 4 for 512*512)\n"
options: .asciiz "Image Editor\n-------------\nfor CONTRAST enter 'C' \nfor BRIGHTNESS enter 'B' \nfor SATURATION enter 'S'\nfor RESIZE IMAGE enter 'R'\nto quit editing 'Q'\n"
wait: .asciiz "image is processing, please wait...\n"
errorOpen: .asciiz "Error opening file\n\n"
errorReading: .asciiz "Error reading file\n\n"
#change this to source of image in your system
filename: .asciiz "/Users/robertmiller/Desktop/bluej.bmp"
samerHere: .asciiz "Great Job!! - Samer\n"
samerHere2: .asciiz "\nGreat Job!! - Samer2\n"
redCoeff: .float .299
blueCoeff: .float .114
greenCoeff: .float .587
numNegOne: .float -1.000
numZero: .float 0.000
desatCoeff: .float 0.500
satCoeff: .float 1.200
numOne:	.float 1.000
numTwo: .float 2.000
numThree: .float 3.000
numFour: .float 4.000
numSixty: .float 60.000
numTwoFive: .float 255.000
numThreeSixty: .float 360.000
buffer: .word  #space to accept user input
header: .space 	54 #54 bytes standard header size - 14 bytes for basic file information & 40 for image info	
satProcess: .asciiz "Saturation has been chosen. Applying now...\n"
desatProcess: .asciiz "Desaturation has been chosen. Applying now...\n"
.text
main: 
	#print loading message to inform user image is loading to display and to wait
	la $a0, loading
	li $v0, 4
	syscall
	#call to open and read in bitmap file then show it on bitmap display
	jal openBMP
	#$s0 and $s1 do not use in calculations; contain important data from openBMP
		#0($s0) image width in pixels
		#4($s0) image height in pixels
		#8($s0) size of image in bytes
		#s1 is address on stack where image data is stored
	#use temp registers for calculations in functions
	
giveOptions:
	#print menu options
	la $a0, options
	li $v0, 4
	syscall

	#get user selection
	li $v0, 8
	la $a0, buffer
	li $a1, 4	
	syscall
	
	#check meaning of user input
	lb $t1, buffer
	beq $t1, 67, contrast
	beq $t1, 66, brightness
	beq $t1, 82, scale
	beq $t1, 83, saturation		#(This is for Saturation 'S')
	beq $t1, 81, end
#################################################################################
saturation:

	#Print saturation options
	la $a0, saturationOptions
	li $v0, 4
	syscall
	
	#read user input
	li $v0, 5
	syscall
	
	#check that number is either 0 or 1
	beq $v0, 0, desat	#user chose 0
	beq $v0, 1, sat	#user chose 1
	j saturation
	
desat:
		
	#Print operation has begun
	la $a0, desatProcess
	li $v0, 4
	syscall
	l.s $f20, desatCoeff
	jal processImage
	
	#move address new image data into argument for display call
	move $a0, $s3
	jal display
	
	#prompt user for editing options again
	j giveOptions
sat:
		
	#Print operation has begun
	la $a0, satProcess
	li $v0, 4
	syscall
	l.s $f20, satCoeff
	
	jal processImage
	
	#move address new image data into argument for display call
	move $a0, $s3
	jal display
	
	#prompt user for editing options again
	j giveOptions
#################################################################################
contrast:
	#print level of contrast option
	li $v0, 4
	la $a0, contrastOptions
	syscall	
	#read user input
	li $v0, 5
	syscall
	
	#check that is number between 1 & 10
	blt $v0, 1, contrast
	bgt $v0, 10, contrast
	#move user selection to argument for processImage call
	move $a3, $v0
	mul $a3, $a3, 10
	
	li $v0, 4
	la $a0, wait
	syscall
	
	jal processImage
	
	#move address new image data into argument for display call
	move $a0, $s3
	jal display
	
	#prompt user for editing options again
	j giveOptions
brightness: 
	#print level of brightness option
	li $v0, 4
	la $a0, brightnessOptions
	syscall	
	#read user input
	li $v0, 5
	syscall
	#check level is between -10 & 10
	blt $v0, -10, brightness
	bgt $v0, 10, brightness
	#move user selection to argument for processImage call
	move $a3, $v0
	mul $a3, $a3, 10
	
	li $v0, 4
	la $a0, wait
	syscall
	
	jal processImage
	#move address of new image data into argument for display call
	move $a0, $s3
	jal display
	#prompt user editing options again
	j giveOptions
	
scale: 
	#print the scale option
	li $v0, 4
	la $a0, scaleImageOptions
	syscall
	
	#read user input
	li $v0, 5
	syscall	
	
	#check scale factor is between 1 and 4
	blt $v0, 1, scale
	bgt $v0, 4, scale
	
	###################
	#move $a0, $v0
	#li $v0, 1
	#syscall
	###################
	
	move $t0, $v0
	move $a0, $s1
	
	beq $t0, 1, scale64 	
	beq $t0, 2, scale128
	beq $t0, 3, scale256
	beq $t0, 4, scale512
	
	#prompt user scale options again
	j giveOptions

scale64:
	
	move $s2, $ra #save return address
	la $t0, 0x10008000 #screen address
	li $t1, 64 #width
	li $t2, 64 #height
	mul $t3, $t1, $t2 #number of pixels (height x width)
	mul $t3, $t3, 32 #corresponds to the top 1/8 of the image at 64x64
	
	add $t0, $t0, $t3 #start address ; t0 used to iterate through screen addresses
	
	li $t8, 0 #count of pixels in width
	mul $t7, $t1, 8 #t7 = 2*(w*4) used for moving to next row : (width in pixel * 4) = size in words , *2 to get to begining of next row
			##iterates from right to left, thus to go to next row, subtract width of entire row twice (get to end of current row and up to next)
	move $t4, $a0	#address of image data on stack, t4 will be used to iterate through it
	add $t3, $zero, 0x10008000

	displayScale64Loop:
		beq $t0, $t3, endScale64Loop #if current address = end address, exit loop
		beq $t8, $t1, nextScale64Row #if count = width, have done all pixels in that row, go to next row
	
	##compose pixel word byte by byte
		lbu $t6, 0($t4)	#blue component 
		lbu $t5, 1($t4)	#green component
		sll $t5, $t5, 8  #move to bits 8-15
		add $t6, $t6, $t5 #store blue in bit 0-7 and green in 8-15 of word
		lbu $t5, 2($t4) #red component
		sll $t5, $t5, 16 #move to bits 16-24
		add $t6, $t6, $t5 #red stored in bits 16-24 of pixel word
	#finished composing pixel word
	
		sw $t6, 0($t0) #store pixel word in current screen address
		add $t4, $t4, 24	#move to next 8th next pixel in image data/ makes 8 instances of image
		add $t8, $t8, 1		#increment count for width position
		add $t0, $t0, 4		#increment to next address
		j displayScale64Loop		
	endScale64Loop:
	jr $s2

	nextScale64Row:
		li $t8, 0	#reset width counter
		sub $t0, $t0, $t7	#go to next row of address
		j displayScale64Loop
	
scale128:
	
	move $s2, $ra #save return address
	la $t0, 0x10008000 #screen address
	li $t1, 128 #width
	li $t2, 128 #height
	mul $t3, $t1, $t2 #number of pixels (height x width)
	mul $t3, $t3, 16 #corresponds to the top 1/4 of the image at 128x128
	
	add $t0, $t0, $t3 #start address ; t0 used to iterate through screen addresses
	
	li $t8, 0 #count of pixels in width
	mul $t7, $t1, 8 #t7 = 2*(w*4) used for moving to next row : (width in pixel * 4) = size in words , *2 to get to begining of next row
			##iterates from right to left, thus to go to next row, subtract width of entire row twice (get to end of current row and up to next)
	move $t4, $a0	#address of image data on stack, t4 will be used to iterate through it
	add $t3, $zero, 0x10008000

	displayScale128Loop:
		beq $t0, $t3, endScale128Loop #if current address = end address, exit loop
		beq $t8, $t1, nextScale128Row #if count = width, have done all pixels in that row, go to next row
	
	##compose pixel word byte by byte
		lbu $t6, 0($t4)	#blue component 
		lbu $t5, 1($t4)	#green component
		sll $t5, $t5, 8  #move to bits 8-15
		add $t6, $t6, $t5 #store blue in bit 0-7 and green in 8-15 of word
		lbu $t5, 2($t4) #red component
		sll $t5, $t5, 16 #move to bits 16-24
		add $t6, $t6, $t5 #red stored in bits 16-24 of pixel word
	#finished composing pixel word
	
		sw $t6, 0($t0) #store pixel word in current screen address
		add $t4, $t4, 12	#move to the 4th next pixel in image data/ makes 4 instances of image
		add $t8, $t8, 1		#increment count for width position
		add $t0, $t0, 4		#increment to next address
		j displayScale128Loop		
	endScale128Loop:
	jr $s2

	nextScale128Row:
		li $t8, 0	#reset width counter
		sub $t0, $t0, $t7	#go to next row of address
		j displayScale128Loop
	
scale256:

	move $s2, $ra #save return address
	la $t0, 0x10008000 #screen address
	li $t1, 256 #width
	li $t2, 256 #height
	mul $t3, $t1, $t2 #number of pixels (height x width)
	mul $t3, $t3, 8 #corresponds to the top 1/2 of the image at 256x256
	
	add $t0, $t0, $t3 #start address ; t0 used to iterate through screen addresses
	
	li $t8, 0 #count of pixels in width
	mul $t7, $t1, 8 #t7 = 2*(w*4) used for moving to next row : (width in pixel * 4) = size in words , *2 to get to begining of next row
			##iterates from right to left, thus to go to next row, subtract width of entire row twice (get to end of current row and up to next)
	move $t4, $a0	#address of image data on stack, t4 will be used to iterate through it
	add $t3, $zero, 0x10008000

	displayScale256Loop:
		beq $t0, $t3, endScale256Loop #if current address = end address, exit loop
		beq $t8, $t1, nextScale256Row #if count = width, have done all pixels in that row, go to next row
	
	##compose pixel word byte by byte
		lbu $t6, 0($t4)	#blue component 
		lbu $t5, 1($t4)	#green component
		sll $t5, $t5, 8  #move to bits 8-15
		add $t6, $t6, $t5 #store blue in bit 0-7 and green in 8-15 of word
		lbu $t5, 2($t4) #red component
		sll $t5, $t5, 16 #move to bits 16-24
		add $t6, $t6, $t5 #red stored in bits 16-24 of pixel word
	#finished composing pixel word
	
		sw $t6, 0($t0) #store pixel word in current screen address
		add $t4, $t4, 6		#move to 2nd next pixel in image data/ 2 instances of image 
		add $t8, $t8, 1		#increment count for width position
		add $t0, $t0, 4		#increment to next address
		j displayScale256Loop		
	endScale256Loop:
	jr $s2

	nextScale256Row:
		li $t8, 0	#reset width counter
		sub $t0, $t0, $t7	#go to next row of address
		j displayScale256Loop
	
scale512:

	move $s2, $ra #save return address
	la $t0, 0x10008000 #screen address
	li $t1, 512 #width
	li $t2, 512 #height
	mul $t3, $t1, $t2 #number of pixels (height x width)
	mul $t3, $t3, 4 #24 bits in a pixel; bitmap display uses one word per pixel
	
	add $t0, $t0, $t3 #start address ; t0 used to iterate through screen addresses
	
	li $t8, 0 #count of pixels in width
	mul $t7, $t1, 8 #t7 = 2*(w*4) used for moving to next row : (width in pixel * 4) = size in words , *2 to get to begining of next row
			##iterates from right to left, thus to go to next row, subtract width of entire row twice (get to end of current row and up to next)
	move $t4, $a0	#address of image data on stack, t4 will be used to iterate through it
	add $t3, $zero, 0x10008000

	displayScale512Loop:
		beq $t0, $t3, endScale512Loop #if current address = end address, exit loop
		beq $t8, $t1, nextScale512Row #if count = width, have done all pixels in that row, go to next row
	
	##compose pixel word byte by byte
		lbu $t6, 0($t4)	#blue component 
		lbu $t5, 1($t4)	#green component
		sll $t5, $t5, 8  #move to bits 8-15
		add $t6, $t6, $t5 #store blue in bit 0-7 and green in 8-15 of word
		lbu $t5, 2($t4) #red component
		sll $t5, $t5, 16 #move to bits 16-24
		add $t6, $t6, $t5 #red stored in bits 16-24 of pixel word
	#finished composing pixel word
	
		sw $t6, 0($t0) #store pixel word in current screen address
		add $t4, $t4, 3		#move to next pixel in image data/ 1 of the image
		add $t8, $t8, 1		#increment count for width position
		add $t0, $t0, 4		#increment to next address
		j displayScale512Loop		
	endScale512Loop:
	jr $s2

	nextScale512Row:
		li $t8, 0	#reset width counter
		sub $t0, $t0, $t7	#go to next row of address
		j displayScale512Loop
	
end:
	#close the program
	li $v0, 10
	syscall
	
# $a0 = red, $a1 = green, $a2 = blue passed in from processImage; $a3 is level passed in from main 
darken:
		subu $a0, $a0, $a3
		bgt $a0, 0, darkerGreen #check component not less than 0, if so set to 0
		li $a0, 0
	darkerGreen:
		subu $a1, $a1, $a3
		bgt $a1, 0, darkerBlue#check component not less than 0, if so set to 0
		li $a1, 0	
	darkerBlue:
		subu $a2, $a2, $a3
		bgt $a2, 0, done #check component not less than 0, if so set to 0
		li $a2, 0
	j done
	
# $a0 = red, $a1 = green, $a2 = blue passed in from processImage; $a3 is level passed in from main
brighten:
		add $a0, $a0, $a3
		bltu $a0, 255, lighterGreen #check component not more than 255, if so set to 255
		li $a0, 255
	lighterGreen:
		add $a1, $a1, $a3
		bltu $a1, 255, lighterBlue #check component not more than 255, if so set to 255
		li $a1, 255
	lighterBlue:
		add $a2, $a2, $a3
		bltu $a2, 255, done #check component not more than 255, if so set to 255
		li $a2, 255
	j done
	
done: 	
	jr $ra

processImage:
	move $s2, $ra #store return address
	
	lw $t1, 8($s0) #size of image data
	move $t2, $s1 #address of current image data on stack, use $t2 to iterate through 
	
	sub $sp, $sp, $t1 #allocate room on stack for new image data 
	add $s3, $sp, $zero #save location on stack for new image data in $s3
	
	move $t8, $s3 #location of new image data on stack, use $t8 to iterate through stack locations

	contrastLoop:
	#extract RGB values
		lbu $a2, 0($t2)	#blue component 
		
		lbu $a1, 1($t2)	#green component
		
		lbu $a0, 2($t2) #red component
	#end extraction
	#check user option & branch accordingly
		lb $t7, buffer
		beq $t7, 67, contrastRGB #if option was contrast
		beq $t7, 66, brightnessRGB
		beq $t7, 83, saturationRGB
	#end user option check
#################################################################################	
	saturationRGB:
		jal RGBtoHSV
		#l.s $f24, desatCoeff	#stored the value 0.5 for computational purposes
		
		mul.s $f3, $f3, $f20 
		#Multiply saturation by 1.5
		jal HSVtoRGB
		
		j store
#################################################################################
	contrastRGB:
		jal contrastPixel
		j store #store new pixel data
	brightnessRGB:
		slti $t3, $a3, 0 #if argument $a3 is less than 0 image is getting darker
		beq $t3, 1, darkenJump #if image is getting darker jump to darken function; else image is getting brighter
		jal brighten
		j store #jump over the darken jump if image got brighter & store new pixel data
	darkenJump:
		mul $a3, $a3, -1 #make $a3 positive so it works correctly with darken function
		jal darken
		mul $a3, $a3, -1 #make negative again so loop continues to work properly, calling darken
	store:
	#store new pixel data 
		sb $a2, 0($t8)
		sb $a1, 1($t8)
		sb $a0, 2($t8)
	#end store new pixel data
	#adjust iterators accordingly
		addi $t8, $t8, 3
		addi $t2, $t2, 3 #increase by three bytes; move to next pixel
		addi $t1, $t1, -3 #decrement size of image data left to iterate through
	#end iterator adjustment
		bne $t1, 0, contrastLoop
	#update current image data with newly edited data
		move $s1, $s3
	#deallocate stack space used to store image data during editing
		lw $t1, 8($s0) #size of image data
		add $sp, $sp, $t1
		
		jr $s2	
		
# $a0 = red, $a1 = green, $a2 = blue from processImage
############################################################################################################
RGBtoHSV:

	#Calculate saturated RGB Value of component
		move $t3, $ra	#return address stored in $t3
		l.s $f31, numZero	#stored the value 0 for computational purposes 
		l.s $f30, numNegOne	#stored the value -1 for computational purposes
		l.s $f29, numTwo	#stored the value 2 for computational purposes
		l.s $f28, numThreeSixty	#stored the value 360 for computational purposes
		l.s $f27, numFour	#stored the value 4 for computational purposes
		l.s $f26, numSixty	#stored the value 60 for computational purposes
		l.s $f24, numTwoFive	#stored the value 255 for computational purposes
		
		mtc1 $a0, $f4 		#red component in $a0
		cvt.s.w $f4, $f4	#Convert to float
		div.s $f4, $f4, $f24	#divides by 255
		
		mtc1 $a1, $f5 		#green component in $a1
		cvt.s.w $f5, $f5	#Convert to float
		div.s $f5, $f5, $f24	#divides by 255
		
		mtc1 $a2, $f6 		#blue component in $a2
		cvt.s.w $f6, $f6	#Convert to float
		div.s $f6, $f6, $f24	#divides by 255
		
		j findMinMax
	
findMinMax:
#Determines Max Value between the three colors
#Stores max in $f0
#Stores min in $f1

	c.le.s $f4, $f5		#check if red < green
	bc1t checkGreenOrBlueMax	#if true, check between green and blue
	bc1f checkRedOrBlueMax		#if false, check between red and blue
	
checkGreenOrBlueMax: 
	c.le.s $f5, $f6		#if green < blue
	bc1t blueMax		#if true, set blue to max 
	bc1f greenMax		#if false, set green to max
checkRedOrBlueMax:		
	c.le.s $f4, $f6		#if red < blue
	bc1t blueMax		#if true, set blue to max 
	bc1f redMax		#if false, set red to max 
#Sets color found to be max
blueMax: 
	mov.s $f0, $f6		#Move color blue to $f0
	j checkRedOrGreenMin			
redMax:	
	mov.s $f0, $f4		#Move color red to $f0
	j checkGreenOrBlueMin	
greenMax: 
	mov.s $f0, $f5		#Move color green to $f0
	j checkRedOrBlueMin

#Since we found max, now we find the min between remaining two colors
checkRedOrGreenMin:
	c.le.s $f4, $f5		#check if red < green
	bc1t redMin		#if true, set red as min
	bc1f greenMin		#if false, set green as min
checkGreenOrBlueMin:
	c.le.s $f5, $f6		#check if green < blue
	bc1t greenMin		#if true, set green as min
	bc1f blueMin		#if false, set blue as min
checkRedOrBlueMin:		
	c.le.s $f4, $f6		#check if red < blue
	bc1t redMin		#if true, set red as min
	bc1f blueMin		#if false, set blue as min
#Set the color found to be min
blueMin:
	mov.s $f1, $f6		#Move color blue to $f1
	j continue
redMin:
	mov.s $f1, $f4		#Move color red to $f1
	j continue
greenMin:
	mov.s $f1, $f5		#Move color green to $f1
	j continue

continue:
#f0 = max, f1 = min, f4 = red, f5 = green, f6 = blue
#Stores delta in $f2
#Stores s in $f3
#Stores h in $f7
#v = max = $f0

	sub.s $f2, $f0, $f1	#delta = max - min
	c.eq.s $f0, $f31 	#check if max = 0
	bc1f maxNotZero		#if(max != 0)
	bc1t else		#if(max = 0)
maxNotZero:
	div.s $f3, $f2, $f0	#s = delta / max
	j checkColor
else: 
	mov.s $f3, $f31		#s = 0
	mov.s $f7, $f30		#h = -1
	jr $t3			#return 
	
checkColor:
	c.eq.s $f4, $f0 	#check if (red == max)
	bc1t YellowMagenta	#if true, jump to YellowMag
	c.eq.s $f5, $f0		#else check if (green == max)
	bc1t CyanYellow		#if true, jump to CyanYellow
	j MagentaCyan		#else, jump to MagentaCyan
back:
	mul.s $f7, $f7, $f26	# h = h * 60
	c.le.s $f7, $f31	#check if ( h < 0)
	bc1t add360		#if true, add 360		
	j return	

add360: 
	add.s $f7, $f7, $f28	#$f28 = 360.000
	j return
YellowMagenta:
	sub.s $f7, $f5, $f6	#h = green - blue
	div.s $f7, $f7, $f2	#h = h / delta
	j back

CyanYellow:
	sub.s $f7, $f6, $f4	#h = blue - red
	div.s $f7, $f7, $f2	#h = h / delta
	add.s $f7, $f7, $f29	#h = h + 2
	j back
	 
MagentaCyan:
	sub.s $f7, $f4, $f5	#h = red - green
	div.s $f7, $f7, $f2	#h = h / delta
	add.s $f7, $f7, $f27	#h = h + 4
	j back
return:
	jr $t3
############################################################################################################
HSVtoRGB:
#f7 = h, f3 = s, f0 = v
#f4 = red, f5 = green, f6 = blue
#store 'i' in $f8
#store 'f' in $f9
#store 'p' in $f10
#store 'q' in $f11
#store 't' in $f12


	move $t3, $ra	#return address stored in $t3
	l.s $f25, numOne	#stored the value 1 for computational purposes
	l.s $f23, numThree	#stored the value 3 for computational purposes
	l.s $f24, numTwoFive
	#0 = f31, 2 = f29, 4 = f27 (already stored)
	
	 c.eq.s $f3, $f31	#check if(s == 0)
	 bc1t achromatic	#if true
	 j evaluate		#else

 achromatic:
 	#red = blue = green == max
 	mov.s $f4, $f0		
 	mov.s $f5, $f0		 
 	mov.s $f6, $f0		
 	jr $t3 		#return 
 evaluate:
 	div.s $f7, $f7, $f26	#h = h / 60 
 	mov.s $f8, $f7		#i = h
 	
 	cvt.w.s $f8, $f8
 	mfc1 $t9, $f8		#This will make the number truncate into an int ( floor )
 	mtc1 $t9, $f8		# 'i' equals floor(h)
 	cvt.s.w $f8, $f8
	sub.s $f9, $f7, $f8	#f = h - i	
	
	#calculate p
		sub.s $f10, $f25, $f3	#p = 1 - s	
		mul.s $f10, $f0, $f10	#p = v * (1 - s) 
	#end calculation of p
	#calculate q
		mul.s $f11, $f3, $f9	# q = s * f
		sub.s $f11, $f25, $f11	#q = (1 - s * f)
		mul.s $f11, $f0, $f11	#q = v * (1-s*f)
	#end calculation of q
	#calculate t
		sub.s $f12, $f25, $f9	#t = 1 -f
		mul.s $f12, $f3, $f12	#t = s * (1 - f)
		sub.s $f12, $f25, $f12	#t = 1 - s *(1 - f)
		mul.s $f12, $f0, $f12	#t = v * (1 - s * (1 - f))
	#end calculation of t
	
	 c.eq.s $f8, $f31 	#if(i==0)
	 bc1t case0
	 c.eq.s $f8, $f25	#if(i==1)
	 bc1t case1
	 c.eq.s $f8, $f29 	#if(i==2)
	 bc1t case2
	 c.eq.s $f8, $f23 	#if(i==3)
	 bc1t case3
	 c.eq.s $f8, $f27 	#if(i==4)
	 bc1t case4
	 
	 #default
	 mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a0, $f0		#r = v	
	 mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10		
	 mfc1 $a1, $f10		#g = p
	 mul.s $f11, $f11, $f24
	 cvt.w.s $f11, $f11
	 mfc1 $a2, $f11		#b = q
	 j checkOver

case0:
	mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a0, $f0		#r = v
	 mul.s $f12, $f12, $f24
	 cvt.w.s $f12, $f12
	 mfc1 $a1, $f12		#g = t
	 mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10
	 mfc1 $a2, $f10		#b = p
	 j checkOver
case1:
	mul.s $f11, $f11, $f24
	 cvt.w.s $f11, $f11
	 mfc1 $a0, $f11		#r = q
	 mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a1, $f0 		#g = v
	 mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10
	 mfc1 $a2, $f10		#b = p
	 j checkOver
case2:
	mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10
	 mfc1 $a0, $f10		#r = p
	 mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a1, $f0 		#g = v
	 mul.s $f12, $f12, $f24
	 cvt.w.s $f12, $f12
	 mfc1 $a2, $f12		#b = t
	 j checkOver
case3:
	 mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10
	 mfc1 $a0, $f10		#r = p
	 mul.s $f11, $f11, $f24
	 cvt.w.s $f11, $f11
	 mfc1 $a1, $f11		#g = q
	 mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a2, $f0 		#b = v
	 j checkOver
case4:
	 mul.s $f12, $f12, $f24
	 cvt.w.s $f12, $f12
	 mfc1 $a0, $f12		#r = t
	 mul.s $f10, $f10, $f24
	 cvt.w.s $f10, $f10
	 mfc1 $a1, $f10		#g = p
	 mul.s $f0, $f0, $f24
	 cvt.w.s $f0, $f0
	 mfc1 $a2, $f0 		#b = v
	 j checkOver
	
#Checks for overflow on values of red, green and blue (must be values between 0 and 255)
checkOver:
	bgt $a0, 255, setR255	#red > 255	
	blt $a0, 0, setR0	#red < 0
	bgt $a1, 255, setG255	#green > 255
	blt $a1, 0, setG0	#green < 0
	bgt $a2, 255, setB255	#blue > 255
	blt $a2, 0, setB0	#blue < 0
	jr $t3			#Values are correct, may return

setR255:
	li $a0, 255
	j checkOver
setR0:
	li $a0, 0
	j checkOver
setG255:
	li $a1, 255
	j checkOver
setG0:
	li $a1, 0
	j checkOver	
setB255:
	li $a2, 255
	j checkOver
setB0:
	li $a2, 0
	j checkOver		 
##################################################################################################
contrastPixel:		
	#calculate contrasted RGB value
		#calc luminance; luminance measure is .299R + .587G + .114B
			#load red coefficient .299 
			l.s $f1, redCoeff
			#convert red component to float
			mtc1 $a0, $f4 #red component in $a0
			cvt.s.w $f4, $f4
			#end conversion
			
			mul.s $f1, $f1, $f4 #.299R
			
			#load blue coefficient .114 
			l.s $f2, blueCoeff
				
			#convert blue component to float
			mtc1 $a2, $f5 #blue
			cvt.s.w $f5, $f5
			#end conversion
			
			mul.s $f2, $f2, $f5 #.114B
			
			#load green coefficient .587 
			l.s $f3, greenCoeff
				
			#convert green component to float
			mtc1 $a1, $f6 #green
			cvt.s.w $f6, $f6
			#end conversion 
			
			mul.s $f3, $f3, $f6 #.587G
			
			add.s $f7, $f1, $f2 #.299R + .114B
			add.s $f7, $f7, $f3 #.299R + .114B + .587G
			
			cvt.w.s $f7, $f7
			mfc1 $t7, $f7
		#end calc luminance ; luminance stores in $t7
		
		#evaluate pixel luminance & adjust accordingly
		move $t3, $ra #store $ra in $r3 so appropriate return can be made after brighten or darken called
		bgtu $t7, 128, brighten #if the pixel is light
		bltu $t7, 128, darken #if pixel is dark
		jr $t3
	#end contrasted RGB value calculation




display:
	move $s2, $ra #save return address
	la $t0, 0x10008000 #screen address
	lw $t1, 0($s0) #width
	lw $t2, 4($s0) #height
	mul $t3, $t1, $t2 #number of pixels (height x width)
	mul $t3, $t3, 4 #24 bits in a pixel; bitmap display uses one word per pixel
	
	add $t0, $t0, $t3 #start address ; t0 used to iterate through screen addresses
	
	li $t8, 0 #count of pixels in width
	mul $t7, $t1, 8 #t7 = 2*(w*4) used for moving to next row : (width in pixel * 4) = size in words , *2 to get to begining of next row
			##iterates from right to left, thus to go to next row, subtract width of entire row twice (get to end of current row and up to next)
	move $t4, $a0	#address of image data on stack, t4 will be used to iterate through it
	add $t3, $zero, 0x10008000

	displayLoop:
		beq $t0, $t3, endLoop #if current address = end address, exit loop
		beq $t8, $t1, nextRow #if count = width, have done all pixels in that row, go to next row
	
	##compose pixel word byte by byte
		lbu $t6, 0($t4)	#blue component 
		lbu $t5, 1($t4)	#green component
		sll $t5, $t5, 8  #move to bits 8-15
		add $t6, $t6, $t5 #store blue in bit 0-7 and green in 8-15 of word
		lbu $t5, 2($t4) #red component
		sll $t5, $t5, 16 #move to bits 16-24
		add $t6, $t6, $t5 #red stored in bits 16-24 of pixel word
	#finished composing pixel word
	
		sw $t6, 0($t0) #store pixel word in current screen address
		add $t4, $t4, 3		#move to next pixel in image data
		add $t8, $t8, 1		#increment count for width position
		add $t0, $t0, 4		#increment to next address
		j displayLoop		
	endLoop:
	jr $s2
#end dispOriginal

nextRow:
	li $t8, 0	#reset width counter
	sub $t0, $t0, $t7	#go to next row of address
j displayLoop


 
storeBitmap:
	#store image data in buffer which is on stack at location $s1
	add $a1, $s1, $zero #address of input buffer - location on stack
	lw $a2, 8($s0) #maximum number of bytes to read - size of image in bytes
	add $a0, $s2, $zero#file descriptor
	li $v0, 14 #syscall for read from file
	syscall				
	bltu $v0, $zero, errReadFile #if $v0 <= 0 error
	jr $ra
#end storeBitmap

extractHeaderDetails:
	la $t0, header 	#move header to $t0 to perform operations on it	
	#check file basic info
		lbu $t1, 0($t0) #first byte of info should be B in ASCII which is 0x42
		lbu $t2, 1($t0) #second byte of info should be M in ASCII which is 0x4D
		bne $t1, 0x42, errReadFile	#if not 0x42 & 0x4D, not correct format of image
		bne $t2, 0x4d, errReadFile	#error reading file

			#bytes 6-9 reserved & should to be 0 for bitmap formatting
			lbu $t1, 6($t0)
			lbu $t2, 7($t0)
			lbu $t3, 8($t0)
			lbu $t4, 9($t0)
			bne $t1, $zero, errReadFile
			bne $t2, $zero, errReadFile
			bne $t3, $zero, errReadFile
			bne $t4, $zero, errReadFile
			#get offset to where image pixel data starts
			lwr $s7, 10($t0)
		#end check basic info
	
		#check bitmap header info
			lbu $t1, 14($t0) #header size, should be 40 for bitmap
			bne $t1, 40, errReadFile
			
			lbu $t1, 28($t0)  #number of bits per pixel, (MIPS uses 24) needs to be 24
			bne $t1, 24, errReadFile
			#lwr to start at address that is not word aligned - lw only works for addresses thatare multiples of 4
			lwr $t1, 18($t0) #width in pixels
	
			lwr $t2, 22($t0) #height in pixels

			#calculate size by multiplying height and width by 3
			mul $t3, $t1, $t2
			mul $t3, $t3, 3
		#end check bitmap header

	#store data obtained
		# $s0 will be the adress located in the stack that will store: size, height, & width
		addi $sp, $sp, -12
		add $s0, $sp, $zero
	
		sw $t1, 0($s0) #image width in pixels
		sw $t2, 4($s0) #image height in pixels
		sw $t3, 8($s0) #size of image in bytes

		#allocate room on stack for image data in bytes, save stack location in $s1
		sub $sp, $sp, $t3
		add $s1, $sp, $zero
	#end store data obtained
	jr $ra
#end extractHeaderDetails	
		
errOpenFile:
	li $v0,4 
	la $a0, errorOpen
	syscall
	j end
#end errOpenFile

errReadFile:
	li $v0,4 
	la $a0, errorReading
	syscall
	j end
#end errReadFile


openBMP:
	#open bitmap file
	move $s5, $ra			#store return address
	la $a0, filename		#load filename into argument
	li $v0, 13			#syscall for opening files
	li $a1, 0			#open file for reading
	li $a2, 0			#mode is ignored (not needed parameter)
	syscall				#returns the descriptor (pointer) of the file in $v0
	bltu $v0, $zero, errOpenFile	#v0 holds file descriptor; if v0 <=0 error occured
	
	move $s6, $v0			#save file descriptor for use of read & close file
	#read header from file
	move $a0, $s6			#a0=v0 which is file descriptor
	la $a1, header 			#copy the file header (header is buffer to load input data to)
	li $a2, 54			#read 54 bytes (size of header)
	li $v0, 14			#syscall for read from file		
	syscall				#return the number of read characters ; if v0 <=0 error occured
	bltu $v0, $zero, errReadFile	# if error occured, handle error	
					#$a0 memory address of file descriptor
	add $s2, $a0, $zero 		#save file descriptor; use in storeBitmap
	jal extractHeaderDetails
		# takes the image details in the header, stores important details on stack, uses $s0 to point to stack location 
		# $s1 holds location on stack allocated with space for image data
		#Output saved info using $s0 and $s1 ; use temp registers for calculations

	jal storeBitmap
	#close file
	li   $v0, 16       # system call for close file
	move $a0, $s6      # file descriptor to close
	syscall 

	move $a0, $s1
	jal display
		#$s0 and $s1 do not use in calculations; contain important data from extractHeaderDetails
		#use temp registers for calculations
		
	jr $s5
